-- Echo server program
module Main (main) where

-- imports
import Control.Concurrent (forkFinally, forkIO)
import qualified Control.Exception as E
import Control.Monad (unless, forever, void)
import qualified Data.ByteString as S
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import Text.Parsec hiding (State, token)
import Text.Parsec.String
import Text.Read (Lexeme(String))

-- Code is modified from starting point at https://hackage.haskell.org/package/network-3.1.2.7/docs/Network-Socket.html
main :: IO ()
main = runTCPServer Nothing "3000" talk
  where
    talk s = do
        msg <- recv s 1024
        forkIO (handleRequest msg s) -- Handles it in a seperate thread
        talk s

handleRequest msg s = do
        unless (S.null msg) $ do
          print msg
          sendAll s msg

-- from the "network-run" package.
runTCPServer :: Maybe HostName -> ServiceName -> (Socket -> IO a) -> IO a
runTCPServer mhost port server = withSocketsDo $ do
    addr <- resolve
    E.bracket (open addr) close loop
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
                | AddStudentRequest Username Password String
                | ChangePasswordRequest Username Password String
                | SetDoodleRequest Username Password
                | GetDoodleRequest Username Password
                | SubscribeRequest Username Password
                | PreferRequest Username Password
                | ExamScheduleRequest                               deriving (Show)


-- Helper functions, taken from Solutions12.hs from the WPO. They take care of whitespaces



token :: Parser a -> Parser a
token p = try $ spaces >> p

keyword :: String -> Parser String
keyword = token . string

symbol = keyword




-- add-teacher parser
addTeacherRequestParser::Parser Request
addTeacherRequestParser = do
    command <- keyword "add-teacher"
    username <- token $ many1 letter :: Parser String
    symbol "@"
    password <- token $ many1 alphaNum
    name <- token $ many1 letter
    return $ AddTeacherRequest username password name
    --return $ command ++ " " ++ username++ " " ++ password ++ " " ++ name
    --let totalthing =  read $ command ++ " " ++  username ++  " " ++ password ++ " " ++ name :: String
    

-- add-student parser
addStudentRequestParser::Parser String
addStudentRequestParser = keyword "add-student"

-- change-password parser
changePasswordRequestParser::Parser String
changePasswordRequestParser = keyword "change-password"

-- set-doodle parser
setDoodleRequestParser::Parser String
setDoodleRequestParser = keyword "set-doodle"

-- get-doodle parser
getDoodleRequestParser::Parser String
getDoodleRequestParser = keyword "get-doodle"

-- subscribe parser
subscribeRequestParser::Parser String
subscribeRequestParser = keyword "subscribe"

-- prefer parser
preferRequestParser::Parser String
preferRequestParser = keyword "prefer"

-- exam-schedule parser
examScheduleRequestParser::Parser String
examScheduleRequestParser = keyword "exam-schedule"

-- Toplevel request parser
parseRequest::Parser Request
parseRequest =  try addTeacherRequestParser 
              --  try addStudentRequestParser <|>
              --  try changePasswordRequestParser <|>
              --  try setDoodleRequestParser <|>
              --  try getDoodleRequestParser <|>
              --  try subscribeRequestParser <|>
              --  try preferRequestParser <|>
              --  try examScheduleRequestParser

request::Parser Request
request = do
    parseRequest
