{-# LANGUAGE OverloadedStrings #-}
module Chester where

import Control.Monad.Trans
import qualified Data.ByteString.Lazy.Char8 as L
import Data.IterIO
import Network
import System.Environment
import System.IO

printInum :: Inum L.ByteString L.ByteString IO a
printInum = mkInumM $ forever $ do
  line <- lineI
  liftIO $ putStrLn $ L.unpack line
  irun $ enumPure line
  where forever m = m >> forever m

loginInum :: Monad m
          => String
          -> String
          -> Iter L.ByteString m a
          -> Inum L.ByteString L.ByteString m b
loginInum nick channel outI = mkInumM $ do
  liftI $ do
    runIterM outI $ Chunk (L.pack $ "NICK " ++ nick ++ "\n") False
    runIterM outI $ Chunk (L.pack $ "USER " ++ nick ++ " 0 * :chesterbot\n") False
    runIterM outI $ Chunk (L.pack $ "JOIN #" ++ channel ++ "\n") False

data IRCCommand
  -- |A private message with sender and message
  = PrivMsg L.ByteString L.ByteString
  | Ping L.ByteString

--parseIRCInum :: Monad m => Inum L.ByteString IRCCommand m a

main :: IO ()
main = do
  server:port:nick:channel:_ <- getArgs
  h <- connectTo server (PortNumber (fromIntegral $ read port))
  hSetBuffering h NoBuffering
  let (outI, inI) = (handleI h, enumHandle h)
  let pipeline = foldl cat inumNull [ loginInum nick channel outI
                                    , printInum
                                    ]
  cat inumNull inI |$ pipeline .| nullI

