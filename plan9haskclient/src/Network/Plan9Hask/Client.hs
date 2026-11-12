{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Network.Plan9Hask.Client where

import Control.Exception qualified
import Data.ByteString qualified
import Data.ByteString.Builder qualified
import Data.List.NonEmpty qualified
import Data.Text qualified
import Data.Text.Encoding qualified
import Data.Word qualified
import Dhall qualified
import Network.Plan9Hask.Msg qualified
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
    Network.Socket.ByteString.Lazy.sendAll socket $ Data.ByteString.Builder.toLazyByteString $ versionMsgData 4192 1
    putStrLn "Sent"

msgWithHeader :: Data.Word.Word32 -> Network.Plan9Hask.Msg.MessageType -> Data.Word.Word16-> Data.ByteString.Builder.Builder
msgWithHeader size msgType tag =
  Data.ByteString.Builder.word32LE size
    <> (Data.ByteString.Builder.word8 $ msgType.getMsgType)
    <> Data.ByteString.Builder.word16LE tag

versionByteString :: Data.ByteString.ByteString
versionByteString = Data.ByteString.toStrict $ Data.ByteString.Builder.toLazyByteString $ plan9Str "9P2000"

versionMsgData :: Data.Word.Word32 -> Data.Word.Word16 -> Data.ByteString.Builder.Builder
versionMsgData mSize mTag =
  (msgWithHeader size Network.Plan9Hask.Msg.tVersion mTag)
    <> Data.ByteString.Builder.word32LE mSize
    <> Data.ByteString.Builder.byteString versionByteString
  where
    size :: Data.Word.Word32 = fromIntegral $ 4 + 1 + 2 + 4 + (Data.ByteString.length versionByteString)

plan9Str :: Data.Text.Text -> Data.ByteString.Builder.Builder
plan9Str str =
  Data.ByteString.Builder.word16LE strLength <> Data.ByteString.Builder.byteString strByteString
  where
    strByteString = Data.Text.Encoding.encodeUtf8 str
    strLength :: Data.Word.Word16 = (fromIntegral (Data.ByteString.length strByteString)) -- Possible overflow
