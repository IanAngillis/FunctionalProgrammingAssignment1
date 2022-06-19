-- Echo server program
{-# LANGUAGE BlockArguments #-} --using a do block as an argument to a function 
module Main (main) where

-- imports
import qualified Assignment1 as AS1
import Control.Concurrent ( forkFinally, forkIO )
import Control.Concurrent.STM
    ( atomically, newTVar, readTVar, writeTVar, TVar )
import qualified Control.Exception as E
import Control.Monad (unless, forever, void)
import qualified Data.ByteString as S
import Network.Socket
    ( setCloseOnExecIfNeeded,
      defaultHints,
      getAddrInfo,
      openSocket,
      withSocketsDo,
      setSocketOption,
      gracefulClose,
      accept,
      bind,
      listen,
      close,
      withFdSocket,
      AddrInfo(addrFlags, addrSocketType, addrAddress),
      AddrInfoFlag(AI_PASSIVE),
      HostName,
      ServiceName,
      SocketOption(ReuseAddr),
      Socket,
      SocketType(Stream) )
import Network.Socket.ByteString (recv, sendAll, send)
import Text.Parsec
    ( alphaNum,
      digit,
      noneOf,
      oneOf,
      spaces,
      string,
      count,
      many1,
      (<|>),
      parse,
      try,
      ParseError )
import Text.Parsec.String ( Parser )
import Text.Read (Lexeme(String))
import System.IO ()
import Data.Either ( fromRight )
import System.Environment ( getArgs )
import qualified Data.ByteString.Char8 as BS
import qualified Data.Map as Map
import Data.Maybe ( fromJust, isNothing )
import Data.List ( elemIndex, sort )
import Data.Word ()
import Data.Bool (Bool)
import GHC.Conc (writeTVar, newTVar)
import System.Random ( getStdGen, newStdGen, Random(randomRs) )
import Data.Time.LocalTime ( ZonedTime )
import Data.Time (UTCTime)
import Data.Time.Format.ISO8601 ()
import Doodle (Doodle(toggle))
import Assignment1 (Timeslot(Timeslot))

-- COMMENTS ON THE PROJECT AS A WHOLE
--"yyyy-mm-ddThh:mm:ss±hh:mm" is the format that will be transformed to ZonedTime
-- Some code in assignment1.hs modified from the code from assingment one so that I was able to recycle quite a bit of code.

-- Data types
data User = User String String [String] Role deriving (Eq, Show)
data Role = STUDENT | TEACHER | ADMIN deriving (Eq, Show)
data State = State (TVar (Map.Map String User)) (TVar (Map.Map String (AS1.MyDoodle ZonedTime)))
data Request =  AddTeacherRequest String String String
                | AddStudentRequest String String String
                | ChangePasswordRequest String String String
                | SetDoodleRequest String String (AS1.MyDoodle ZonedTime) -- username password doodle slots
                | GetDoodleRequest String String String
                | SubscribeRequest String String String
                | PreferRequest String String String (AS1.Timeslot ZonedTime)
                | ExamScheduleRequest String String
                | InvalidRequest                deriving (Show)

-- Data type to easily print an exam
data Exam = Exam String ZonedTime ZonedTime [String]
instance Show Exam where
    show (Exam name starttime endtime participants) = name ++ ": " ++ show starttime ++ "/" ++ show endtime


-- Create a new empty state when the server boots
newState::String -> String  -> IO State
newState username password = atomically (do {
                                let u = Map.insert username (User username password [] ADMIN ) Map.empty
                                    d = Map.empty
                                ;users  <- newTVar u
                                ;doodles <- newTVar d
                                ;return (State users doodles)
})

-- Creates a new password --
getpassword::IO [Char]
getpassword = do
    gen <- getStdGen
    newStdGen
    return (take 4 $ randomRs ('A','z') gen :: [Char])

-- Handles the parsed requests using STM. One procedure for every kind of request.
-- The layout is quite the same for every transaction. We read the data we need from state, modify it and write it back

-- handle add-teacher
handleParsedRequest::Request -> State -> IO String
handleParsedRequest (AddTeacherRequest admin password teacher) (State ux dx) = do
    newpass <- getpassword
    atomically (do {
        users <- readTVar ux
        ;let (User n p xs r) = fromJust(Map.lookup admin users)
        ;if User n p xs r == User admin password xs ADMIN
            then do
                if Map.member teacher users
                    then do
                        return "id-taken"
                    else do
                        writeTVar ux $ Map.insert teacher (User teacher newpass [] TEACHER) users -- Add new teacher to the map of users
                        return ("ok " ++ newpass)
            else
                return "wrong-login"
})

-- handle add-student
handleParsedRequest (AddStudentRequest admin password student) (State ux dx) = do
    newpass <- getpassword
    atomically (do {
        users <- readTVar ux
        ;let (User n p xs r) = fromJust (Map.lookup admin users)
         ;if User n p xs r == User admin password xs ADMIN
            then do
                if Map.member student users
                    then do
                        return "id-taken"
                    else do
                        writeTVar ux $ Map.insert student (User student newpass [] STUDENT) users -- Add new student to the map of users
                        return ("ok " ++ newpass)
            else
                return "wrong-login"
})

-- handle change-password
handleParsedRequest (ChangePasswordRequest user password newpass) (State ux dx) = do
    atomically (do {
        users <- readTVar ux
        ;let (User n p xs r) = fromJust(Map.lookup user users)
        ;if User n p xs r == User user password xs r
            then do
                writeTVar ux $ Map.insert n (User n newpass xs r) users --Update user with new password
                return "ok"
            else
                return "wrong-login"
})

-- handle set-doodle
handleParsedRequest (SetDoodleRequest user password (AS1.MyDoodle doodle slots)) (State ux dx) = do
    atomically (do {
        users <- readTVar ux
        ;doodles <- readTVar dx
        ;let (User n p xs r) = fromJust(Map.lookup user users)
        ;if User n p xs r == User user password xs TEACHER -- If the user is a teacher
            then do
                if isNothing (Map.lookup doodle doodles)    -- If the doodle does not exist yet
                    then do
                        writeTVar ux $ Map.insert n (User n p (doodle:xs) r) users
                        writeTVar dx $ Map.insert doodle (AS1.MyDoodle doodle (sort slots)) doodles
                        return $ "ok"
                    else do
                        let (AS1.MyDoodle title slots) = fromJust $ Map.lookup doodle doodles
                        if title `elem` xs      -- If it exists and the user is the creator of the original one, overwrite it.
                            then do
                                writeTVar dx $ Map.insert doodle (AS1.MyDoodle doodle (sort slots)) doodles
                                return "ok"
                            else do
                                return "id-taken"
            else
                return "wrong-login"
})

-- handle get-doodle
handleParsedRequest (GetDoodleRequest user password doodle) (State ux dx) = do
    atomically (do {
        users <- readTVar ux
        ;doodles <- readTVar dx
        ;let (User n p xs r) = fromJust(Map.lookup user users)
        ;if User n p xs r == User user password xs r
            then do
                if isNothing (Map.lookup doodle doodles)    -- If the doodle does not exist
                    then do
                        return "no-such-id"
                    else do -- The doodle does exist - fetch it
                        let (AS1.MyDoodle title slots) = fromJust $ Map.lookup doodle doodles
                        return $ "ok " ++ show slots
            else
                return "wrong-login"
})

-- handle subscribe
handleParsedRequest (SubscribeRequest user password doodle) (State ux dx) = do
    atomically (do {
        users <- readTVar ux
        ;doodles <- readTVar dx
        ;let (User n p xs r) = fromJust(Map.lookup user users)
        ;if User n p xs r == User user password xs STUDENT
            then do
                if isNothing (Map.lookup doodle doodles)
                    then do
                        return "no-such-id"
                    else do
                        let (AS1.MyDoodle title slots) = fromJust $ Map.lookup doodle doodles
                        if title `elem` xs
                            then do
                                return "ok"
                            else do
                                writeTVar ux $ Map.insert n (User n p (title:xs) r) users
                                return "ok"
            else
                return "wrong-login"
})

-- handle prefer-doodle
handleParsedRequest (PreferRequest user pass doodle slot) (State ux dx) = do
    atomically (do {
        users <- readTVar ux
        ;doodles <- readTVar dx
        ;let (User n p xs r) = fromJust(Map.lookup user users)
        ;if User n p xs r == User user pass xs STUDENT
            then do
                if doodle `elem` xs --Is subscribed to doodle
                    then do
                        if isNothing (Map.lookup doodle doodles)
                            then do
                                return "no-such-id"
                            else do -- check if the slot exists
                                let (AS1.MyDoodle title slots) = fromJust $ Map.lookup doodle doodles
                                if isNothing (elemIndex slot slots)
                                    then do
                                        return "no-such-slot"
                                    else do
                                        let idx = fromJust $ elemIndex slot slots
                                        writeTVar dx $ Map.insert doodle  (toggle user idx (AS1.MyDoodle title (AS1.removeParticipantFromAllSlots user slots))) doodles
                                        return "ok"

                    else do
                        return "not-subscribed"
            else do
                return "wrong-login"
    })

-- handle exam-schedule
handleParsedRequest (ExamScheduleRequest user pass) (State ux dx) = do
    atomically (do {
        users <- readTVar ux
        ;doodles <- readTVar dx
        ;let (User n p xs r) = fromJust(Map.lookup user users)
        ;if User n p xs r == User user pass xs r
            then do
                let doodleTaggedSlots = convertSlots .  convertToValues $ Map.toList doodles            -- Convert doodles to indivual exam tuples with their timeslots
                    schedule = optimalTimeSlot doodleTaggedSlots $ convertToValues $ Map.toList users   -- Get the most optimal one

                if null schedule then return "no-possible-exam-schedule" else return ("ok { " ++ formatExams (map convertToExams schedule) ++ " }")
        else do
            return "wrong-login"
        })
-- Invalid request - not part of the procoticol but it serves to let the client know there was an invalid request
handleParsedRequest InvalidRequest s = return "invalid request"

-- This logic handles the formation of the exam-schedules.
countStudents :: Foldable t => (a1, b, c, t a2) -> Int -> Int
countStudents (n, s, e, p) c = c + length p

-- format the exam to properly display the schedule to the user
formatExams::[Exam] -> String
formatExams [] = ""
formatExams [exam] = show exam
formatExams (x:xs) = show x ++ ", " ++ formatExams xs

-- Turn the exam tuples into exam types 
convertToExams :: (String, ZonedTime, ZonedTime, [String]) -> Exam
convertToExams (n, s, e, p) = Exam n s e p

convertToValues :: [(a, b)] -> [b]
convertToValues = map snd

convertSlots :: [AS1.MyDoodle ZonedTime] -> [[(String, ZonedTime, ZonedTime, [String])]]
convertSlots = map getSlots

getSlots::AS1.MyDoodle ZonedTime -> [(String, ZonedTime , ZonedTime, [String] )]
getSlots (AS1.MyDoodle title slots) = map (tagSlots title) slots

-- Adds a tag to the timeslot to know what the doodle is
tagSlots::String -> Timeslot ZonedTime -> (String, ZonedTime, ZonedTime, [String])
tagSlots tag (AS1.Timeslot s e p) = (tag, s, e, p)

-- Calculates the most optimal exam-eschedule.
-- The main idea is to generate all the possible exam schedules and then filter those good ones out so that we end up with a list of accepted schedules.
-- From that list of accepted schedules, we look for the optimal one. A (most) optimal schedule is one where the preferred slots are maximized.
optimalTimeSlot::[[(String, ZonedTime, ZonedTime, [String])]] ->  [User] -> [(String, ZonedTime, ZonedTime, [String])]
optimalTimeSlot doodles users = let teachers = filter (hasRole TEACHER) users
                                    students = filter (hasRole STUDENT) users
                                    acceptedschedules = [schedule | schedule  <-  sequence doodles, teachersDoNotOverlap students schedule teachers , studentsDoNotOverlap schedule students ]
                        in if null acceptedschedules then [] else snd . maximum $ zip (map (foldr countStudents 0 ) acceptedschedules) acceptedschedules


teachersDoNotOverlap::[User] -> [(String, ZonedTime, ZonedTime, [String])] -> [User] -> Bool
teachersDoNotOverlap students schedule = all (checkNoOverlaps . teacherGivesExam students schedule)

studentsDoNotOverlap::[(String, ZonedTime, ZonedTime, [String])] -> [User] ->  Bool
studentsDoNotOverlap schedule = all (checkNoOverlaps . studentPrefersExam schedule)


teacherGivesExam::[User] -> [(String, ZonedTime, ZonedTime, [String])] -> User -> [(String, ZonedTime, ZonedTime, [String])]
teacherGivesExam students schedule (User name _ exams TEACHER) = [(n, s, e, p) | (n, s, e, p) <- schedule, n `elem` exams]

studentsAreSubscribedToExam::[User]->String->Bool
studentsAreSubscribedToExam students exam = any (studentSubscribedToExam exam) students

studentSubscribedToExam::String->User->Bool
studentSubscribedToExam exam (User _ _ subs _) = exam `elem` subs


studentPrefersExam::[(String, ZonedTime, ZonedTime, [String])] -> User -> [(String, ZonedTime, ZonedTime, [String])]
studentPrefersExam schedule (User name _ exams STUDENT) = [(n, s, e, p) | (n, s, e, p) <- schedule, name `elem` p || n `elem` exams]


hasRole::Role -> User -> Bool
hasRole role (User _ _ _ r) = r == role

checkNoOverlaps::[(String, ZonedTime, ZonedTime, [String])] -> Bool
checkNoOverlaps exams = let overlapresults =  [ doesnotoverlap e1 e2 | e1 <- exams, e2 <- exams, e1 < e2]
                        in and overlapresults

doesnotoverlap :: Ord t => (a1, t, t, d1) -> (a2, t, t, d2) -> Bool
doesnotoverlap (n1, s1, e1, p1) (n2, s2, e2, p2) = AS1.doesNotOverlap s1 e1 s2 e2

-- The server code is modifed from https://hackage.haskell.org/package/network-3.1.2.7/docs/Network-Socket.html which as used as starting point
main :: IO ()
main = runTCPServer Nothing "3000" talk
  where
    talk s state = do
        msg <- recv s 1024
        forkIO (handleRequest msg s state) -- Handles it in a seperate thread
        talk s state

-- Handles the request that is accepted on the port
handleRequest msg s state=
        unless (S.null msg) $ do
            print msg -- Print to the server console for debugging purposes
            -- Parse the request, if we do not have a correct parsing we return and invalidrequest
            let request = fromRight InvalidRequest $ parse getRequest "" $ BS.unpack msg -- We use the let so that we can pure code, allows us to use the parser monad within the IO monad
            response <- handleParsedRequest request state
            sendAll s $ BS.pack response

-- from the "network-run" package.
runTCPServer :: Maybe HostName -> ServiceName -> (Socket -> State -> IO a) -> IO a
runTCPServer mhost port server = withSocketsDo $ do
    args <- getArgs
    state <- newState (head args) (head $ tail args) -- Command line arguments
    addr <- resolve
    E.bracket (open addr) close (loop state)  -- Control.Exception
  where
    resolve = do
        let hints = defaultHints {
                addrFlags = [AI_PASSIVE]
              , addrSocketType = Stream
              }
        head <$> getAddrInfo (Just hints) mhost (Just port)
    open addr = E.bracketOnError (openSocket addr) close $ \sock -> do
        setSocketOption sock ReuseAddr 1
        withFdSocket sock setCloseOnExecIfNeeded
        bind sock $ addrAddress addr
        listen sock 1024
        return sock
    loop state sock  = forever $ E.bracketOnError (accept sock) (close . fst)
        $ \(conn, _peer) -> void $
            forkFinally (server conn state) (const $ gracefulClose conn 5000)

-- END of the modified code.

-- PARSER CODE

year::Parser String
year = count 4 digit

month::Parser String
month = count 2 digit

day::Parser String
day = count 2 digit

hours::Parser String
hours = count 2 digit

minutes::Parser String
minutes = count 2 digit

seconds::Parser String
seconds = count 2 digit

--"yyyy-mm-ddThh:mm:ss±hh:mm" is the format that will be transformed to ZonedTime
zonedTimeParser::Parser ZonedTime
zonedTimeParser = do
    year <- year
    symbol "-"
    month <- month
    symbol "-"
    day <- day
    symbol "T"
    hour <- hours
    symbol ":"
    minute <- minutes
    symbol ":"
    second <- seconds
    e <- oneOf "+-"
    e_hour <- hours
    symbol ":"
    e_minutes <- minutes
    let time = year++"-"++month++"-"++day++" "++hour++":"++minute++":"++second++ [e] ++e_hour++":"++e_minutes
        zonedTime = read time :: ZonedTime
    return zonedTime

slotParser::Parser (AS1.Timeslot ZonedTime)
slotParser = do
    slot1 <- zonedTimeParser
    symbol "/"
    slot2 <- zonedTimeParser
    return (AS1.Timeslot slot1 slot2 [])

slotParserL::Parser [AS1.Timeslot ZonedTime]
slotParserL = do
    slot1 <- zonedTimeParser
    symbol "/"
    slot2 <- zonedTimeParser
    return [AS1.Timeslot slot1 slot2 []]

slotParser'::Parser (AS1.Timeslot ZonedTime)
slotParser' = do
    slot1 <- zonedTimeParser
    symbol "/"
    slot2 <- zonedTimeParser
    oneOf ",]"
    return (AS1.Timeslot slot1 slot2 [])

slot :: Parser (AS1.Timeslot ZonedTime)
slot = token $ slotParser

multipleslots::Parser [AS1.Timeslot ZonedTime]
multipleslots = do
    symbol "["
    try (many1 slotParser')

slots::Parser [AS1.Timeslot ZonedTime]
slots = token $ multipleslots

-- Taken from Solutions12.hs from the WPO. They take care of whitespaces
token :: Parser a -> Parser a
token p = try $ spaces >> p

keyword :: String -> Parser String
keyword = token . string

symbol = keyword

word = token $ many1 alphaNum
anyword = token $ many1 $ noneOf " "

-- With more time, I would've extracted the authentication part out of the parser so that it would be its own thing can be plugged in like Lego. 
-- add-teacher parser
addTeacherRequestParser::Parser Request
addTeacherRequestParser = do
    command <- keyword "add-teacher"
    username <- keyword "admin"
    symbol "@"
    password <- word
    AddTeacherRequest username password <$> word


-- add-student parser
addStudentRequestParser::Parser Request
addStudentRequestParser = do
    command <- keyword "add-student"
    username <- keyword "admin"
    symbol "@"
    password <- word
    AddStudentRequest username password <$> word

-- change-password parser
changePasswordRequestParser::Parser Request
changePasswordRequestParser = do
    command <- keyword "change-password"
    username <- word
    symbol "@"
    password <- word
    ChangePasswordRequest username password <$> word

-- set-doodle parser
setDoodleRequestParser::Parser Request
setDoodleRequestParser = do
    command <- keyword "set-doodle"
    username <- word
    symbol "@"
    password <- word
    doodle <- word
    SetDoodleRequest username password . AS1.MyDoodle doodle <$> slots

-- get-doodle parser
getDoodleRequestParser::Parser Request
getDoodleRequestParser = do
    command <- keyword "get-doodle"
    username <- word
    symbol "@"
    password <- word
    GetDoodleRequest username password <$> word

-- subscribe parser
subscribeRequestParser::Parser Request
subscribeRequestParser = do
    command <- keyword "subscribe"
    username <- word
    symbol "@"
    password <- word
    SubscribeRequest username password <$> word

-- prefer parser
preferRequestParser::Parser Request
preferRequestParser = do
    command <- keyword "prefer"
    username <- word
    symbol "@"
    password <- word
    doodle <- word
    PreferRequest username password doodle <$> slot

-- exam-schedule parser
examScheduleRequestParser::Parser Request
examScheduleRequestParser = do
    command <- keyword "exam-schedule"
    username <- word
    symbol "@"
    ExamScheduleRequest username <$> word

-- request parser
parseRequest::Parser Request
parseRequest =  try addTeacherRequestParser <|>
                try addStudentRequestParser <|>
                try changePasswordRequestParser <|>
                try setDoodleRequestParser <|>
                try getDoodleRequestParser <|>
                try subscribeRequestParser <|>
                try preferRequestParser <|>
                try examScheduleRequestParser

--toplevel
getRequest::Parser Request
getRequest =
    parseRequest

-- Testing code
test1 :: Either ParseError Request
test1 = parse getRequest "" "add-teacher admin@1234 walter"
test2 :: Either ParseError Request
test2 = parse getRequest "" "add-student admin@1234 jesse"
test3 :: Either ParseError Request
test3 = parse getRequest "" "change-password walter@qf1Qs foobar"
test4 :: Either ParseError Request
test4 = parse getRequest "" "get-doodle walter@qf1Qs Cooking"
test5 :: Either ParseError Request
test5 = parse getRequest "" "set-doodle walter@wfum Cooking [2022-01-04T14:00:00+01:00/2022-01-04T16:00:00+01:00,2022-01-04T13:00:00+01:00/2022-01-04T15:00:00+01:00]"
test6 :: Either ParseError Request
test6 = parse getRequest "" "subscribe karen@umfl Cooking"
test7 :: Either ParseError Request
test7 = parse getRequest "" "prefer jos@flyb Cooking 2022-01-04T14:00:00+01:00/2022-01-04T16:00:00+01:00"
test8 :: Either ParseError Request
test8 = parse getRequest "" "exam-schedule karen@fsko"


-- Tests functionality
testsuite :: IO ()
testsuite = do
          print test1
          print test2
          print test3
          print test4
          print test5
          print test6
          print test7
          print test8