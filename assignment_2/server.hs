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
import Data.Word
import Data.Bool (Bool)
import GHC.Conc (writeTVar, newTVar)
import System.Random
import Data.Time.LocalTime
import Data.Time (UTCTime)
import Data.Time.Format.ISO8601

-- EXTRA DATA TYPES USED IN THE ASSIGNMENT
data User = User String String [String] Role deriving (Eq, Show)
data Role = STUDENT | TEACHER | ADMIN deriving (Eq, Show)
data State = State (TVar (Map.Map String User)) (TVar (Map.Map String (AS1.MyDoodle ZonedTime)))
data Request =  AddTeacherRequest String String String
                | AddStudentRequest String String String
                | ChangePasswordRequest String String String
                | SetDoodleRequest String String String String -- username password doodle slots
                | GetDoodleRequest String String String
                | SubscribeRequest String String String
                | PreferRequest String String String String
                | ExamScheduleRequest String String
                | InvalidRequest                deriving (Show)


-- HELPER FUNCTION FOR VARIOUS THINGS LIKE PARSER AND STM

-- Create a new empty state when the server boots
newState::String -> String  -> IO State
newState username password = atomically (do {
                                let u = Map.insert username (User username password [] ADMIN ) Map.empty
                                    d = Map.empty
                                ;users  <- newTVar u
                                ;doodles <- newTVar d
                                ;return (State users doodles)
})

-- Creates a new password
getpassword::IO [Char]
getpassword = do
    gen <- getStdGen
    newStdGen
    return (take 4 $ randomRs ('0','z') gen :: [Char])

-- handleParsedRequest: enables the administrator to add a teacher
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

-- AddStudentRequest: enables the administrator to add a student
handleParsedRequest (AddStudentRequest admin password student) (State ux dx) = do
    newpass <- getpassword
    atomically (do {
        users <- readTVar ux
        ;let (User n p xs r) = fromJust (Map.lookup admin users)
        ;if User n p xs r == User admin password xs ADMIN
            then do
                writeTVar ux $ Map.insert student (User student newpass [] STUDENT) users
                return ("ok " ++ newpass)
            else
                return "wrong-login"
})

-- ChangePassWordRequest: enables the user to change their password
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
                        return (show slots)
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




-- //TODO enable a user to get the current best exam schedule



handleParsedRequest InvalidRequest s = return "invalid request"

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
    SetDoodleRequest username password doodle <$> anyword

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
    PreferRequest username password doodle <$> anyword

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

test1 = parse getRequest "" "add-teacher admin@1234 walter"
test2 = parse getRequest "" "add-student admin@1234 jesse"
test3 = parse getRequest "" "change-password walter@qf1Qs foobar"
test4 = parse getRequest "" "get-doodle walter@qf1Qs Cooking"
test5 = parse getRequest "" "set-doodle walter@foobar Cooking [2022-01-04T14:00+01:00/2022-01-04T16:00+01,2022-01-04T13:00+01:00/2022-01-04T15:00+01:00]"
test6 = parse getRequest "" "subscribe jesse@2bYo Cooking"
test7 = parse getRequest "" "prefer jesse@2bYo Cooking 2022-01-04T13:00+01:00/2022-01-04T15:00+01:00"
test8 = parse getRequest "" "exam-schedule jesse@2bYo"


-- Tests functionality
testsuite = do
          print test1
          print test2
          print test3
          print test4
          print test5
          print test6
          print test7
          print test8
