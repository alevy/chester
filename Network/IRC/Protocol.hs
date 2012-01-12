{-# LANGUAGE OverloadedStrings #-}

module Network.IRC.Protocol where

import Prelude hiding (words, unwords)

import qualified Data.ByteString.Lazy.Char8 as L
import Data.ListLike
import Data.ListLike.String

type From = L.ByteString
type To = L.ByteString
type Msg = L.ByteString

data IRCCommand
  -- |A private message with sender and message
  = PrivMsg From To Msg
  | Ping Msg
  | Empty
  deriving (Show)

ircCommandFromLine :: L.ByteString -> Maybe IRCCommand
ircCommandFromLine line = go $ words line
  where go (from:"PRIVMSG":to:prms) = Just $ PrivMsg from to $ unwords prms
        go (from:"PING":prms) = Just $ Ping $ unwords prms
        go _ = Nothing
