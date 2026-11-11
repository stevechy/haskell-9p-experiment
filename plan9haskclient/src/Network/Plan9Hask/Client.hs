{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.Plan9Hask.Client where

import Control.Exception qualified
import Data.ByteString qualified
import Data.ByteString.Builder qualified
import Data.List.NonEmpty qualified
import Data.Text qualified
import Data.Text.Encoding qualified
import Data.Word qualified
import Dhall qualified
import Network.Socket qualified
import Network.Socket.ByteString.Lazy qualified

data SizeOnly = SizeOnly {msgSize :: Data.Word.Word32} deriving (Dhall.Generic, Show)

clientHints = Network.Socket.defaultHints {Network.Socket.addrSocketType = Network.Socket.Stream}

connect addr = Control.Exception.bracketOnError
  (Network.Socket.openSocket addr)
  Network.Socket.close
  $ \socket -> do
    Network.Socket.connect socket $ Network.Socket.addrAddress addr
    return socket

withConnection host service handler = do
  addrs :: Data.List.NonEmpty.NonEmpty Network.Socket.AddrInfo <- (Network.Socket.getAddrInfo (Just clientHints) (Just host) (Just service))
  Control.Exception.bracket (connect $ Data.List.NonEmpty.head addrs) Network.Socket.close handler

runClient = do
  withConnection "127.0.0.1" "8564" $ \socket -> do
    let sizeBytes = Data.ByteString.Builder.word32LE 31337
    Network.Socket.ByteString.Lazy.sendAll socket $ Data.ByteString.Builder.toLazyByteString sizeBytes
    putStrLn "Sent"

msgWithHeader size msgType = Data.ByteString.Builder.word32LE size <> Data.ByteString.Builder.word8 msgType

plan9Str :: Data.Text.Text -> Data.ByteString.Builder.Builder
plan9Str str =
  Data.ByteString.Builder.word16LE strLength <> Data.ByteString.Builder.byteString strByteString
  where
    strByteString = Data.Text.Encoding.encodeUtf8 str
    strLength :: Data.Word.Word16 = (fromIntegral (Data.ByteString.length strByteString))  -- Possible overflow
