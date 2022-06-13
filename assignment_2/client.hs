{-# LANGUAGE OverloadedStrings #-}
-- Echo client program
module Main (main) where

import qualified Control.Exception as E
import qualified Data.ByteString.Char8 as C
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import Data.Word

main :: IO ()
main = runTCPClient "127.0.0.1" "3000" $ \s -> do
    sendAll s "add-teacher admin@1234 walter"
    msg <- recv s 1024
    putStr "Received: "
    C.putStrLn msg

-- from the "network-run" package.
runTCPClient :: HostName -> ServiceName -> (Socket -> IO a) -> IO a
runTCPClient host port client = withSocketsDo $ do
    addr <- resolve
    E.bracket (open addr) close client
  where
    resolve = do
        let hints = defaultHints { addrSocketType = Stream }
        head <$> getAddrInfo (Just hints) (Just host) (Just port)
    open addr = E.bracketOnError (openSocket addr) close $ \sock -> do
        connect sock $ addrAddress addr
        return sock




charToWord8 :: Char -> Word8
charToWord8 = toEnum . fromEnum

convertStringToWord8 ::[Char] -> [Word8]
convertStringToWord8 = map charToWord8