{-# LANGUAGE OverloadedRecordDot #-}

module Network.Plan9Hask.HandlerParser where

import Network.Plan9Hask.Msg qualified as Msg
import Network.Plan9Hask.Parser qualified as Parser
import Data.Word qualified
import Data.Text qualified
import Data.ByteString qualified

data Plan9Callback a = Plan9Callback {
   handleTVersion :: Msg.Tag -> Data.Word.Word32 -> Data.Text.Text -> a,
   handleTAuth :: Msg.Tag -> Msg.Fid -> Msg.Username -> Data.Text.Text -> a,
   handleTFlush:: Msg.Tag -> Msg.Tag -> a,
   handleTAttach :: Msg.Tag -> Msg.Fid -> Msg.AuthFid -> Msg.Username -> Data.Text.Text -> a,
   handleTWalk :: Msg.Tag -> Msg.Fid -> Msg.Fid -> Msg.VarArgByteLength -> a,
   handleTOpen   :: Msg.Tag -> Msg.Fid -> Msg.OpenMode -> a,
   handleTCreate :: Msg.Tag -> Msg.Fid -> Data.Text.Text -> Msg.Permissions -> Msg.CreateMode -> a,
   handleTRead   :: Msg.Tag -> Msg.Fid -> Msg.Offset -> Msg.ReadCount -> a,
   handleTWrite  :: Msg.Tag -> Msg.Fid -> Msg.Offset -> Data.ByteString.ByteString -> a,
   handleTClunk  :: Msg.Tag -> Msg.Fid -> a,
   handleTRemove :: Msg.Tag -> Msg.Fid -> a,
   handleTStat   :: Msg.Tag -> Msg.Fid -> a,
   handleTWstat  :: Msg.Tag -> Msg.Fid -> Msg.Stat -> a,
   handleParseError :: Parser.ParseError -> a
}

handleMsgBody :: Plan9Callback a -> Data.ByteString.ByteString -> a
handleMsgBody callbacks msgBytes
    | Data.ByteString.length msgBytes < 1 + 2 = callbacks.handleParseError Parser.OutOfBytes
    | msgType <- (Data.ByteString.index msgBytes 0),
      tag <- Parser.parseTag msgBytes =
        handleMsgType callbacks msgType tag msgBytes

handleMsgType :: Plan9Callback a -> Data.Word.Word8 -> Msg.Tag -> Data.ByteString.ByteString -> a
handleMsgType callbacks msgType tag msgBytes
  | msgType == Msg.tVersion.getMsgType = tVersionCallback callbacks tag msgBytes
  | otherwise = callbacks.handleParseError Parser.UnknownMsgType

tVersionCallback :: Plan9Callback a -> Msg.Tag -> Data.ByteString.ByteString -> a
tVersionCallback callbacks tag msgBytes
    | Data.ByteString.length msgBytes < 1 + 2 + 4 = callbacks.handleParseError Parser.OutOfBytes
    | otherwise = case Parser.parsePlan9Str $ Data.ByteString.drop (1 + 2 + 4) msgBytes of
      Right versionStr -> callbacks.handleTVersion tag (Parser.convertLeWord32 msgBytes [3, 4, 5, 6]) versionStr
      Left err -> callbacks.handleParseError err
