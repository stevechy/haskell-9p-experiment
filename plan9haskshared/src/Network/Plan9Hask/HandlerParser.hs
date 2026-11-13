{-# LANGUAGE OverloadedRecordDot #-}

module Network.Plan9Hask.HandlerParser where

import Data.ByteString qualified
import Data.Text qualified
import Data.Word qualified
import Network.Plan9Hask.Msg qualified as Msg
import Network.Plan9Hask.Parser qualified as Parser

data Plan9Callback a = Plan9Callback
  { handleTVersion :: Msg.Tag -> Data.Word.Word32 -> Data.Text.Text -> a,
    handleTAuth :: Msg.Tag -> Msg.AuthFid -> Msg.Username -> Msg.AttachName -> a,
    handleTFlush :: Msg.Tag -> Msg.Tag -> a,
    handleTAttach :: Msg.Tag -> Msg.Fid -> Msg.AuthFid -> Msg.Username -> Msg.AttachName -> a,
    handleTWalk :: Msg.Tag -> Msg.Fid -> Msg.Fid -> Msg.VarArgByteLength -> a,
    handleTOpen :: Msg.Tag -> Msg.Fid -> Msg.OpenMode -> a,
    handleTCreate :: Msg.Tag -> Msg.Fid -> Data.Text.Text -> Msg.Permissions -> Msg.CreateMode -> a,
    handleTRead :: Msg.Tag -> Msg.Fid -> Msg.Offset -> Msg.ReadCount -> a,
    handleTWrite :: Msg.Tag -> Msg.Fid -> Msg.Offset -> Data.ByteString.ByteString -> a,
    handleTClunk :: Msg.Tag -> Msg.Fid -> a,
    handleTRemove :: Msg.Tag -> Msg.Fid -> a,
    handleTStat :: Msg.Tag -> Msg.Fid -> a,
    handleTWstat :: Msg.Tag -> Msg.Fid -> Msg.Stat -> a,
    handleParseError :: Parser.ParseError -> a
  }

messageTypeSize = 1

messageTagSize = 2

messageTypeAndTagSize = 1 + 2

handleMsgBody :: Plan9Callback a -> Data.ByteString.ByteString -> a
handleMsgBody callbacks msgBytes
  | Data.ByteString.length msgBytes < messageTypeAndTagSize = callbacks.handleParseError Parser.OutOfBytes
  | msgType <- (Data.ByteString.index msgBytes 0),
    tag <- Parser.parseTag msgBytes =
      handleMsgType callbacks msgType tag msgBytes

handleMsgType :: Plan9Callback a -> Data.Word.Word8 -> Msg.Tag -> Data.ByteString.ByteString -> a
handleMsgType callbacks msgType tag msgBytes
  | msgType == Msg.tVersion.getMsgType = tVersionCallback callbacks tag msgBytes
  | msgType == Msg.tAuth.getMsgType = tAuthCallback callbacks tag msgBytes
  | msgType == Msg.tFlush.getMsgType = tFlushCallback callbacks tag msgBytes
  | msgType == Msg.tAttach.getMsgType = tAttachCallback callbacks tag msgBytes
  | msgType == Msg.tClunk.getMsgType = fidOnlyCallback (callbacks.handleTClunk) (callbacks.handleParseError) tag msgBytes
  | msgType == Msg.tRemove.getMsgType = fidOnlyCallback (callbacks.handleTRemove) (callbacks.handleParseError) tag msgBytes
  | msgType == Msg.tStat.getMsgType = fidOnlyCallback (callbacks.handleTStat) (callbacks.handleParseError) tag msgBytes
  | otherwise = callbacks.handleParseError Parser.UnknownMsgType

tVersionCallback :: Plan9Callback a -> Msg.Tag -> Data.ByteString.ByteString -> a
tVersionCallback callbacks tag msgBytes
  | Data.ByteString.length msgBytes < messageTypeAndTagSize + 4 = callbacks.handleParseError Parser.OutOfBytes
  | otherwise = case Parser.parsePlan9Str $ Data.ByteString.drop (messageTypeAndTagSize + 4) msgBytes of
      Right versionStr -> callbacks.handleTVersion tag (Parser.convertLeWord32 msgBytes [3, 4, 5, 6]) versionStr
      Left err -> callbacks.handleParseError err

tAuthCallback :: Plan9Callback a -> Msg.Tag -> Data.ByteString.ByteString -> a
tAuthCallback callbacks tag msgBytes
  | Data.ByteString.length msgBytes < messageTypeAndTagSize + 4 + 2 + 2 = callbacks.handleParseError Parser.OutOfBytes
  | otherwise =
      let parse = do
            afid <- Right (Msg.AuthFid $ Parser.convertLeWord32 msgBytes [3, 4, 5, 6])
            let remainingBytes = Data.ByteString.drop (messageTypeAndTagSize + 4) msgBytes
            usernameResult <- fmap (mapParsedElement Msg.Username) $ Parser.parsePlan9StrWithRemainder $ remainingBytes
            attachName <- fmap Msg.AttachName $ Parser.parsePlan9Str $ usernameResult.remainder
            return $ callbacks.handleTAuth tag afid (usernameResult.parsedElement) attachName
       in case parse of
            (Right value) -> value
            (Left err) -> callbacks.handleParseError err

tFlushCallback :: Plan9Callback a -> Msg.Tag -> Data.ByteString.ByteString -> a
tFlushCallback callbacks tag msgBytes
  | Data.ByteString.length msgBytes < 1 + 2 + 2 + 2 = callbacks.handleParseError Parser.OutOfBytes
  | otherwise = callbacks.handleTFlush tag (Msg.Tag $ Parser.convertLeWord16 msgBytes [3, 4])

tAttachCallback :: Plan9Callback a -> Msg.Tag -> Data.ByteString.ByteString -> a
tAttachCallback callbacks tag msgBytes
  | Data.ByteString.length msgBytes < messageTypeAndTagSize + 4 + 4 + 2 + 2 = callbacks.handleParseError Parser.OutOfBytes
  | otherwise =
      let parse = do
            fid <- Right (Msg.Fid $ Parser.convertLeWord32 msgBytes [3, 4, 5, 6])
            afid <- Right (Msg.AuthFid $ Parser.convertLeWord32 msgBytes [7, 8, 9, 10])
            let remainingBytes = Data.ByteString.drop (messageTypeAndTagSize + 4 + 4) msgBytes
            usernameResult <- fmap (mapParsedElement Msg.Username) $ Parser.parsePlan9StrWithRemainder $ remainingBytes
            attachName <- fmap Msg.AttachName $ Parser.parsePlan9Str $ usernameResult.remainder
            return $ callbacks.handleTAttach tag fid afid (usernameResult.parsedElement) attachName
       in case parse of
            (Right value) -> value
            (Left err) -> callbacks.handleParseError err

mapParsedElement :: (a -> b) -> Parser.ParseResult a -> Parser.ParseResult b
mapParsedElement f result = result { Parser.parsedElement = f result.parsedElement }

tOpenCallback :: Plan9Callback a -> Msg.Tag -> Data.ByteString.ByteString -> a
tOpenCallback callbacks tag msgBytes
    | Data.ByteString.length msgBytes < messageTypeAndTagSize + 4 + 1 = callbacks.handleParseError Parser.OutOfBytes
    | otherwise =
        let parse = do
                fid <- Right (Msg.Fid $ Parser.convertLeWord32 msgBytes [3, 4, 5, 6])
                mode <- Right (Msg.OpenMode $ Data.ByteString.index msgBytes 7)
                return $ callbacks.handleTOpen tag fid mode
        in case parse of
            (Right value) -> value
            (Left err) -> callbacks.handleParseError err

fidOnlyCallback :: (Msg.Tag -> Msg.Fid -> a) -> (Parser.ParseError -> a) -> Msg.Tag -> Data.ByteString.ByteString -> a
fidOnlyCallback callback errCallback tag msgBytes
    | Data.ByteString.length msgBytes < messageTypeAndTagSize + 4 = errCallback Parser.OutOfBytes
    | otherwise = callback tag (Msg.Fid $ Parser.convertLeWord32 msgBytes [3, 4, 5, 6])
