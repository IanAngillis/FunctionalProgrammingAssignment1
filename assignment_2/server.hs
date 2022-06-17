-- Echo server program
{-# LANGUAGE BlockArguments #-}
module Main (main) where

-- imports
import qualified Assignment1 as AS1
import Control.Concurrent
import Control.Concurrent.STM
import qualified Control.Exception as E
import Control.Monad (unless, forever, void)
import qualified Data.ByteString as S
import Network.Socket
import Network.Socket.ByteString (recv, sendAll, send)
import Text.Parsec hiding (State, token)
import Text.Parsec.String
import Text.Read (Lexeme(String))
import System.IO
import Data.Either
import System.Environment
import qualified Data.ByteString.Char8 as BS
import qualified Data.Map as Map
import Data.Maybe
import Data.List
import Data.Word
import Data.Bool (Bool)
import GHC.Conc (writeTVar, newTVar)
import System.Random
import Data.Time.LocalTime
import Data.Time (UTCTime)
import Data.Time.Format.ISO8601
import Language.Haskell.TH (doublePrimL)
import Doodle (Doodle(toggle))
import Assignment1 (Timeslot(Timeslot))

-- EXTRA DATA TYPES USED IN THE ASSIGNMENT
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


-- HELPER FUNCTION FOR VARIOUS THINGS LIKE PARSER AND STM
-- TODO HANDLE CREATION OF DOODLES AND TIMESLOTS IN THE PARSER
-- TODO Make a function that returns Parser ZonedTime

-- Create a new empty state when the server boots
newState::String -> String  -> IO State
newState username password = atomically (do {
                                let u = Map.insert username (User username password [] ADMIN ) Map.empty
                                    d = Map.empty
                                ;users  <- newTVar u
                                ;doodles <- newTVar d
                                ;return (State users doodles)
})

-- Creates a new password -- //TODO needs to produce alphanumerical characters
getpassword::IO [Char]
getpassword = do
    gen <- getStdGen
    newStdGen
    return (take 4 $ randomRs ('a','z') gen :: [Char])

-- handleParsedRequest: enables the administrator to add a teacher DONE
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
                        writeTVar ux $ Map.insert teacher (User teacher newpass [] TEACHER) users
                        return ("ok " ++ newpass)
            else
                return "wrong-login"
})

-- AddStudentRequest: enables the administrator to add a student DONE
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
                        writeTVar ux $ Map.insert student (User student newpass [] STUDENT) users
                        return ("ok " ++ newpass)
            else
                return "wrong-login"
})

-- ChangePassWordRequest: enables the user to change their password DONE
handleParsedRequest (ChangePasswordRequest user password newpass) (State ux dx) = do
    atomically (do {
        users <- readTVar ux
        ;let (User n p xs r) = fromJust(Map.lookup user users)
        ;if User n p xs r == User user password xs r
            then do
                writeTVar ux $ Map.insert n (User n newpass xs r) users
                return "ok"
            else
                return "wrong-login"
})

-- //TODO SetDoodleRequest: enables teachers to create new exams
{-
Step 1. Get Users
Step 2. Get Doodles
Stem 3. If the doodle does not exist yet, then create the doodle and add it to the doodle list of doodles from the teacher.
Step 4. If the teacher already defined a doodle for the exam, the new doodle should overwrite the old one.
Step 5. If another teacher already claimed the exam identifier, the server should return the message "id-taken".

-}
handleParsedRequest (SetDoodleRequest user password (AS1.MyDoodle doodle slots)) (State ux dx) = do
    atomically (do {
        users <- readTVar ux                                    -- Read the users
        ;doodles <- readTVar dx                                 -- Read the doodle
        ;let (User n p xs r) = fromJust(Map.lookup user users)  -- Get the user
        ;if User n p xs r == User user password xs TEACHER       -- If the user login checks out
            then do
                if isNothing (Map.lookup doodle doodles) -- The doodle does not exist yet, then create the doodle and att it to the doodle list of doodles from the teacher
                    then do
                        writeTVar ux $ Map.insert n (User n p (doodle:xs) r) users
                        writeTVar dx $ Map.insert doodle (AS1.MyDoodle doodle (sort slots)) doodles
                        return $ "ok"
                    else do -- The doodle does exist. Check if it is from the teacher. If it is, overwrite, else 
                        let (AS1.MyDoodle title slots) = fromJust $ Map.lookup doodle doodles
                        if title `elem` xs
                            then do
                                writeTVar dx $ Map.insert doodle (AS1.MyDoodle doodle (sort slots)) doodles -- TODO populate slots
                                return "ok"
                            else do
                                return "id-taken"
            else
                return "wrong-login"
})


-- GetDoodleRequest: enables the user to change get a doodle.
-- TODO proper timeslot formatting
handleParsedRequest (GetDoodleRequest user password doodle) (State ux dx) = do
    atomically (do {
        users <- readTVar ux
        ;doodles <- readTVar dx
        ;let (User n p xs r) = fromJust(Map.lookup user users)
        ;if User n p xs r == User user password xs r
            then do
                if isNothing (Map.lookup doodle doodles)
                    then do
                        return "no-such-id"
                    else do
                        let (AS1.MyDoodle title slots) = fromJust $ Map.lookup doodle doodles
                        return $ "ok " ++ show slots
            else
                return "wrong-login"
})

-- Subscribe to an exam.
-- TODO can be subscribed mutliple times to the same exam
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

-- //TODO prefer an exam slot
-- GET THE INDEX AND CALL UPDATE FROM THE TYPE CLASS
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
                            else do
                                let (AS1.MyDoodle title slots) = fromJust $ Map.lookup doodle doodles
                                    idx = fromJust $ elemIndex slot slots
                                writeTVar dx $ Map.insert doodle  (toggle user idx (AS1.MyDoodle title (AS1.removeParticipantFromAllSlots user slots))) doodles
                                return "ok"
                    else do
                        return "not subscribed"
            else do
                return "wrong-login"
    })



-- //TODO enable a user to get the current best exam schedule
-- Use sequence for this, it combines all the stff together 1 by 1.
-- For every thing, run a check and accept/reject acoordingly
-- Check all the accepted ones and seek the most optimal one???
-- https://stackoverflow.com/questions/14471876/with-a-list-of-lists-combine-each-element-of-each-list-with-each-other-element
handleParsedRequest (ExamScheduleRequest user pass) (State ux dx) = do
    atomically (do {
        users <- readTVar ux
        ;doodles <- readTVar dx
        let doodleTaggedSlots = map getSlots doodles
            optimalTimeSlot = calculatOptimalSchedule doodleTaggedSlots

        ;return "no exam schedule yet"
        })


handleParsedRequest InvalidRequest s = return "invalid request"

getSlots::AS1.MyDoodle ZonedTime -> [(String, ZonedTime , ZonedTime, [String] )]
getSlots (AS1.MyDoodle title slots) = map (tagSlots title) slots

-- Adds a tag to the timeslot to know what the doodle is
tagSlots::String -> Timeslot ZonedTime -> (String, ZonedTime, ZonedTime, [String])
tagSlots tag (Timeslot s e p) = (tag, s, e, p)

-- Eerste filter: voor elke leerkracht, verzamel alle timeslots van de examens die hij afneemt, is er een overlap -- FAAL
-- Tweede filter: voor elke student, verzamel alle timeslots waar die student aan deelneemt, is er een overlap - faal
-- Lijst met goede schedules - Kies de optimale (die waar het meeste studenten aan deelnemen in totaal)
optimalTimeSlot::[(String, ZonedTime, ZonedTime, [String])] -> 
optimalTimeSlot = 


-- Code is modified from starting point at https://hackage.haskell.org/package/network-3.1.2.7/docs/Network-Socket.html
main :: IO ()
main = runTCPServer Nothing "3000" talk
  where
    talk s state = do
        msg <- recv s 1024
        forkIO (handleRequest msg s state) -- Handles it in a seperate thread
        talk s state

-- Handles the request that is accepted on the port
-- TODO sends to all, change to receiver
handleRequest msg s state=
        unless (S.null msg) $ do
            print msg

            -- Parse the request, if we do not have a correct parsing we return and invalidrequest
            let request = fromRight InvalidRequest $ parse getRequest "" $ BS.unpack msg -- We use the let so that we can pure code, allows us to use the parser monad
            response <- handleParsedRequest request state
            sendAll s $ BS.pack response

-- from the "network-run" package.
runTCPServer :: Maybe HostName -> ServiceName -> (Socket -> State -> IO a) -> IO a
runTCPServer mhost port server = withSocketsDo $ do
    state <- newState "admin" "1234"
    addr <- resolve
    --state <- atomically (return newTVar $ Map.insert "admin" "1234" Map.empty)
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
            -- 'forkFinally' alone is unlikely to fail thus leaking @conn@,
            -- but 'E.bracketOnError' above will be necessary if some
            -- non-atomic setups (e.g. spawning a subprocess to handle
            -- @conn@) before proper cleanup of @conn@ is your case
            forkFinally (server conn state) (const $ gracefulClose conn 5000)


-- Create the data type request to differentiate between different request

type Username = String
type Password = String



doodle = "Cooking [2022-12-25T16:00:00+01:00/2022-12-25T18:00:00+01:00, 2022-12-25T18:00:00 +01:00/2022-12-25T20:00:00+01:00]"
date="2022-12-25T16:00:00+01:00"




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


--"yyyy-mm-ddThh:mm:ss[.sss]±hh:mm "
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

-- Helper functions, taken from Solutions12.hs from the WPO. They take care of whitespaces
token :: Parser a -> Parser a
token p = try $ spaces >> p

keyword :: String -> Parser String
keyword = token . string

symbol = keyword


 --do
 --   command <- keyword "add-teacher"
  --  username <- token $ many1 letter :: Parser String
   -- symbol "@"
    --password <- token $ many1 alphaNum
    --name <- token $ many1 letter
    --return $ AddTeacherRequest username password name

word = token $ many1 alphaNum
anyword = token $ many1 $ noneOf " "

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

{-
doodleParser::Parser (MyDoodle ZonedTime)
doodleParser = do
    doodlename <- word
    timeslots <- timeslotsParser
    return (MyDoodle doodlename timeslots)

timeslotsParser::Parser [Timeslot]
timeslotsParser = try singleTime
-}

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
test5 = parse getRequest "" "set-doodle ian@foobar Cooking [2022-01-04T14:00:00+01:00/2022-01-04T16:00:00+01:00,2022-01-04T13:00:00+01:00/2022-01-04T15:00:00+01:00]"
test6 :: Either ParseError Request
test6 = parse getRequest "" "subscribe jesse@2bYo Cooking"
test7 :: Either ParseError Request
test7 = parse getRequest "" "prefer jesse@2bYo Cooking 2022-01-04T13:00:00+01:00/2022-01-04T15:00:00+01:00"
test8 :: Either ParseError Request
test8 = parse getRequest "" "exam-schedule jesse@2bYo"


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




-- fromRight [] $ parse slots "" "[2022-12-25T18:00:00+01:00/2022-12-25T20:00:00+01:00,2022-12-25T16:00:00+01:00/2022-12-25T18:00:00+01:00]"