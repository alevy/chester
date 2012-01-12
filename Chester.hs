{-# LANGUAGE OverloadedStrings #-}
module Chester where

import Control.Monad
import Control.Monad.Trans
import qualified Data.ByteString.Lazy.Char8 as L
import Data.IterIO
import Data.List.Split
import Network
import Network.IRC.Protocol
import System.Environment
import System.IO

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

parseIRCInum :: Inum L.ByteString [IRCCommand] IO a
parseIRCInum = mkInumM $ forever $ do
  line <- lineI
  irun $ enumPure $
    case ircCommandFromLine line of
      Nothing -> []
      Just cmd -> [cmd]

processPing :: MonadIO m => Iter L.ByteString m a -> Inum [IRCCommand] [IRCCommand] m a
processPing outI = mkInumP pullupResid $ do
  next <- headI
  case next of
    Ping msg -> do
      runIterM outI $ Chunk (L.pack $ "PONG " ++ (L.unpack msg)) False
      liftIO $ putStrLn "PIIIIIIIINNNNNNNGGGG"
      return []
    _ -> return [next]

processPrivMsgInum :: Inum [IRCCommand] [IRCCommand] IO a
processPrivMsgInum = mkInumP pullupResid $ do
  next <- headI
  case next of
    PrivMsg from to msg -> do
      liftIO $ putStrLn $ (show next)
      return []
    _ -> return [next]

main :: IO ()
main = do
  server:port:nick:channel:_ <- getArgs
  h <- connectTo server (PortNumber (fromIntegral $ read port))
  hSetBuffering h NoBuffering
  let (outI, inI) = (handleI h, enumHandle h)
  let pipeline = foldl cat inumNull [ loginInum nick channel outI
                                    , inumhLog stdout
                                    ]
  cat inumNull inI |$ pipeline .| parseIRCInum .| processPrivMsgInum .| processPing outI .| nullI
