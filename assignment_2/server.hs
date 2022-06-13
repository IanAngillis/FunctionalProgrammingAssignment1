-- Echo server program
{-# LANGUAGE BlockArguments #-}
module Main (main) where

-- imports
import Control.Concurrent (forkFinally, forkIO)
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


import Data.Word

-- https://stackoverflow.com/questions/10623424/haskell-how-to-convert-char-to-word8
charToWord8 :: Char -> Word8
charToWord8 = toEnum . fromEnum

convertStringToWord8 ::[Char] -> [Word8]
convertStringToWord8 = map charToWord8




-- Code is modified from starting point at https://hackage.haskell.org/package/network-3.1.2.7/docs/Network-Socket.html
main :: IO ()
main = runTCPServer Nothing "3000" talk
  where
    talk s = do
        msg <- recv s 1024
        forkIO (handleRequest msg s) -- Handles it in a seperate thread
        talk s

-- Handles the request that is accepted on the port
-- TODO sends to all, change to receiver
handleRequest msg s = do
        unless (S.null msg) $ do
          print msg
          -- Parse the request, if we do not have a correct parsing we return and invalidrequest
          let request = fromRight InvalidRequest $ parse getRequest "" $ BS.unpack msg -- We use the let so that we can pure code, allows us to use the parser monad
              response = case  request of
                                (AddTeacherRequest username password name)              -> "handled add-teacher"
                                (AddStudentRequest username password name)              -> "handled add-student"
                                (ChangePasswordRequest username password newpassword)   -> "handle change-password"
                                (SetDoodleRequest username password doodle times)       -> "handle set-doodle"
                                (GetDoodleRequest username password doodle)             -> "handle get-doodle"
                                (SubscribeRequest username password doodle)             -> "handle subscribe"
                                (PreferRequest username password doodle time)           -> "handle prefer"
                                (ExamScheduleRequest username password)                 -> "handle exam-schedule"
                                InvalidRequest                                          ->  "not found"
          print response
          sendAll s $ BS.pack response


-- from the "network-run" package.
runTCPServer :: Maybe HostName -> ServiceName -> (Socket -> IO a) -> IO a
runTCPServer mhost port server = withSocketsDo $ do
    addr <- resolve
    E.bracket (open addr) close loop -- Control.Exception
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
    loop sock = forever $ E.bracketOnError (accept sock) (close . fst)
        $ \(conn, _peer) -> void $
            -- 'forkFinally' alone is unlikely to fail thus leaking @conn@,
            -- but 'E.bracketOnError' above will be necessary if some
            -- non-atomic setups (e.g. spawning a subprocess to handle
            -- @conn@) before proper cleanup of @conn@ is your case
            forkFinally (server conn) (const $ gracefulClose conn 5000)


-- Create the data type request to differentiate between different request

type Username = String
type Password = String

data Request =  AddTeacherRequest String String String
                | AddStudentRequest String String String
                | ChangePasswordRequest String String String
                | SetDoodleRequest String String String String
                | GetDoodleRequest String String String
                | SubscribeRequest String String String
                | PreferRequest String String String String
                | ExamScheduleRequest String String
                | InvalidRequest                deriving (Show)


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
getRequest = do
    parseRequest

-- Users code
users = Map.empty


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