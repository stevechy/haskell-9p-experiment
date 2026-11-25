{-# LANGUAGE OverloadedRecordDot #-}

module Network.Plan9Hask.HandlerParser where

import Data.ByteString qualified
import Data.Text qualified
import Data.Word qualified
import Network.Plan9Hask.Msg qualified as Msg
import Network.Plan9Hask.Parser qualified as Parser

data RequestHandler a = RequestHandler
  { handleTVersion :: Msg.Tag -> Data.Word.Word32 -> Data.Text.Text -> a,
    handleTAuth :: Msg.Tag -> Msg.AuthFid -> Msg.Username -> Msg.AttachName -> a,
    handleTFlush :: Msg.Tag -> Msg.Tag -> a,
    handleTAttach :: Msg.Tag -> Msg.Fid -> Msg.AuthFid -> Msg.Username -> Msg.AttachName -> a,
    handleTWalk :: Msg.Tag -> Msg.Fid -> Msg.Fid -> Msg.ArrayElementCount -> [Data.Text.Text] -> a,
    handleTOpen :: Msg.Tag -> Msg.Fid -> Msg.OpenMode -> a,
    handleTCreate :: Msg.Tag -> Msg.Fid -> Data.Text.Text -> Msg.Permissions -> Msg.OpenMode -> a,
    handleTRead :: Msg.Tag -> Msg.Fid -> Msg.Offset -> Msg.ByteCount -> a,
    handleTWrite :: Msg.Tag -> Msg.Fid -> Msg.Offset -> Msg.ByteCount -> Data.ByteString.ByteString -> a,
    handleTClunk :: Msg.Tag -> Msg.Fid -> a,
    handleTRemove :: Msg.Tag -> Msg.Fid -> a,
    handleTStat :: Msg.Tag -> Msg.Fid -> a,
    handleTWstat :: Msg.Tag -> Msg.Fid -> Msg.VarArgByteCount -> Msg.Stat -> a,
    handleParseError :: Parser.ParseError -> a
  }

handleMsgBody :: RequestHandler a -> Data.ByteString.ByteString -> a
handleMsgBody callbacks msgBytes
  | Data.ByteString.length msgBytes < Msg.messageTypeAndTagSize = callbacks.handleParseError Parser.OutOfBytes
  | msgType <- (Data.ByteString.index msgBytes 0),
    tag <- Parser.parseTag msgBytes =
      handleMsgType callbacks msgType tag msgBytes

handleMsgType :: RequestHandler a -> Data.Word.Word8 -> Msg.Tag -> Data.ByteString.ByteString -> a
handleMsgType callbacks msgType tag msgBytes
  | msgType == Msg.tVersion.getMsgType = tVersionCallback callbacks tag msgBytes
  | msgType == Msg.tAuth.getMsgType = tAuthCallback callbacks tag msgBytes
  | msgType == Msg.tFlush.getMsgType = tFlushCallback callbacks tag msgBytes
  | msgType == Msg.tAttach.getMsgType = tAttachCallback callbacks tag msgBytes
  | msgType == Msg.tWalk.getMsgType = tWalkCallback callbacks tag msgBytes
  | msgType == Msg.tOpen.getMsgType = tOpenCallback callbacks tag msgBytes
  | msgType == Msg.tCreate.getMsgType = tCreateCallback callbacks tag msgBytes
  | msgType == Msg.tRead.getMsgType = tReadCallback callbacks tag msgBytes
  | msgType == Msg.tWrite.getMsgType = tWriteCallback callbacks tag msgBytes
  | msgType == Msg.tClunk.getMsgType = fidOnlyCallback (callbacks.handleTClunk) (callbacks.handleParseError) tag msgBytes
  | msgType == Msg.tRemove.getMsgType = fidOnlyCallback (callbacks.handleTRemove) (callbacks.handleParseError) tag msgBytes
  | msgType == Msg.tStat.getMsgType = fidOnlyCallback (callbacks.handleTStat) (callbacks.handleParseError) tag msgBytes
  | otherwise = callbacks.handleParseError Parser.UnknownMsgType

tVersionCallback :: RequestHandler a -> Msg.Tag -> Data.ByteString.ByteString -> a
tVersionCallback callbacks tag msgBytes
  | Data.ByteString.length msgBytes < Msg.messageTypeAndTagSize + 4 = callbacks.handleParseError Parser.OutOfBytes
  | otherwise = case Parser.parsePlan9Str $ Data.ByteString.drop (Msg.messageTypeAndTagSize + 4) msgBytes of
      Right versionStr -> callbacks.handleTVersion tag (Parser.convertLeWord32 msgBytes [3, 4, 5, 6]) versionStr
      Left err -> callbacks.handleParseError err

tAuthCallback :: RequestHandler a -> Msg.Tag -> Data.ByteString.ByteString -> a
tAuthCallback callbacks tag msgBytes
  | Data.ByteString.length msgBytes < Msg.messageTypeAndTagSize + 4 + 2 + 2 = callbacks.handleParseError Parser.OutOfBytes
  | otherwise =
      let parse = do
            afid <- Right (Msg.AuthFid $ Parser.convertLeWord32 msgBytes [3, 4, 5, 6])
            let remainingBytes = Data.ByteString.drop (Msg.messageTypeAndTagSize + 4) msgBytes
            usernameResult <- fmap (mapParsedElement Msg.Username) $ Parser.parsePlan9StrWithRemainder $ remainingBytes
            attachName <- fmap Msg.AttachName $ Parser.parsePlan9Str $ usernameResult.remainder
            return $ callbacks.handleTAuth tag afid (usernameResult.parsedElement) attachName
       in case parse of
            (Right value) -> value
            (Left err) -> callbacks.handleParseError err

tFlushCallback :: RequestHandler a -> Msg.Tag -> Data.ByteString.ByteString -> a
tFlushCallback callbacks tag msgBytes
  | Data.ByteString.length msgBytes < 1 + 2 + 2 + 2 = callbacks.handleParseError Parser.OutOfBytes
  | otherwise = callbacks.handleTFlush tag (Msg.Tag $ Parser.convertLeWord16 msgBytes [3, 4])

tAttachCallback :: RequestHandler a -> Msg.Tag -> Data.ByteString.ByteString -> a
tAttachCallback callbacks tag msgBytes
  | Data.ByteString.length msgBytes < Msg.messageTypeAndTagSize + 4 + 4 + 2 + 2 = callbacks.handleParseError Parser.OutOfBytes
  | otherwise =
      let parse = do
            fid <- Right (Msg.Fid $ Parser.convertLeWord32 msgBytes [3, 4, 5, 6])
            afid <- Right (Msg.AuthFid $ Parser.convertLeWord32 msgBytes [7, 8, 9, 10])
            let remainingBytes = Data.ByteString.drop (Msg.messageTypeAndTagSize + 4 + 4) msgBytes
            usernameResult <- fmap (mapParsedElement Msg.Username) $ Parser.parsePlan9StrWithRemainder $ remainingBytes
            attachName <- fmap Msg.AttachName $ Parser.parsePlan9Str $ usernameResult.remainder
            return $ callbacks.handleTAttach tag fid afid (usernameResult.parsedElement) attachName
       in case parse of
            (Right value) -> value
            (Left err) -> callbacks.handleParseError err

mapParsedElement :: (a -> b) -> Parser.ParseResult a -> Parser.ParseResult b
mapParsedElement f result = result { Parser.parsedElement = f result.parsedElement }

tOpenCallback :: RequestHandler a -> Msg.Tag -> Data.ByteString.ByteString -> a
tOpenCallback callbacks tag msgBytes
    | Data.ByteString.length msgBytes < Msg.messageTypeAndTagSize + 4 + 1 = callbacks.handleParseError Parser.OutOfBytes
    | otherwise =
        let parse = do
                fid <- Right (Msg.Fid $ Parser.convertLeWord32 msgBytes [3, 4, 5, 6])
                mode <- Right (Msg.OpenMode $ Data.ByteString.index msgBytes 7)
                return $ callbacks.handleTOpen tag fid mode
        in case parse of
            (Right value) -> value
            (Left err) -> callbacks.handleParseError err

tCreateCallback :: RequestHandler a -> Msg.Tag -> Data.ByteString.ByteString -> a
tCreateCallback callbacks tag msgBytes
    | Data.ByteString.length msgBytes < Msg.messageTypeAndTagSize + 4 + 2 + 4 + 1 = callbacks.handleParseError Parser.OutOfBytes
    | otherwise =
        let parse = do
                let
                    fixedPrefixElementBytes = Msg.messageTypeAndTagSize + 4
                    fid = (Msg.Fid $ Parser.convertLeWord32 msgBytes [3, 4, 5, 6])
                nameResult <- Parser.parsePlan9StrWithRemainder (Data.ByteString.drop fixedPrefixElementBytes msgBytes)
                let bytesAfterName = nameResult.remainder
                if (Data.ByteString.length bytesAfterName) <  4 + 1
                    then Left $ Parser.OutOfBytes
                    else Right ()
                permissions <- Right $ Msg.Permissions $ Parser.convertLeWord32 bytesAfterName [0, 1, 2, 3]
                mode <- Right $ Msg.OpenMode $ Data.ByteString.index bytesAfterName 4
                return $ callbacks.handleTCreate tag fid nameResult.parsedElement permissions mode
        in case parse of
            (Right value) -> value
            (Left err) -> callbacks.handleParseError err

tWalkCallback :: RequestHandler a -> Msg.Tag -> Data.ByteString.ByteString -> a
tWalkCallback callbacks tag msgBytes
    | Data.ByteString.length msgBytes < Msg.messageTypeAndTagSize + 4 + 4 + 2 = callbacks.handleParseError Parser.OutOfBytes
    | otherwise =
        let parse = do
                let fixedElementBytes = Msg.messageTypeAndTagSize + 4 + 4 + 2
                    fid = (Msg.Fid $ Parser.convertLeWord32 msgBytes [3, 4, 5, 6])
                    newFid = (Msg.Fid $ Parser.convertLeWord32 msgBytes [7, 8, 9, 10])
                    walkNameElementCount = (Msg.ArrayElementCount $ Parser.convertLeWord16 msgBytes [11, 12])
                walkNames <- Parser.parsePlan9StrArray (Data.ByteString.drop fixedElementBytes msgBytes) walkNameElementCount
                return $ callbacks.handleTWalk tag fid newFid walkNameElementCount walkNames
        in case parse of
            (Right value) -> value
            (Left err) -> callbacks.handleParseError err

tReadCallback :: RequestHandler a -> Msg.Tag -> Data.ByteString.ByteString -> a
tReadCallback callbacks tag msgBytes
    | Data.ByteString.length msgBytes < Msg.messageTypeAndTagSize + 4 + 8 + 4 = callbacks.handleParseError Parser.OutOfBytes
    | otherwise =
        let parse = do
                fid <- Right (Msg.Fid $ Parser.convertLeWord32 msgBytes [3, 4, 5, 6])
                offset <- Right (Msg.Offset $ Parser.convertLeWord64 msgBytes [7, 8, 9, 10, 11, 12, 13, 14])
                count <- Right (Msg.ByteCount $ Parser.convertLeWord32 msgBytes [15,16,17,18])
                return $ callbacks.handleTRead tag fid offset count
        in case parse of
            (Right value) -> value
            (Left err) -> callbacks.handleParseError err

tWriteCallback :: RequestHandler a -> Msg.Tag -> Data.ByteString.ByteString -> a
tWriteCallback callbacks tag msgBytes
    | Data.ByteString.length msgBytes < Msg.messageTypeAndTagSize + 4 + 8 + 4 = callbacks.handleParseError Parser.OutOfBytes
    | otherwise =
        let parse = do
                let fixedElementBytes = Msg.messageTypeAndTagSize + 4 + 8 + 4
                fid <- Right (Msg.Fid $ Parser.convertLeWord32 msgBytes [3, 4, 5, 6])
                offset <- Right (Msg.Offset $ Parser.convertLeWord64 msgBytes [7, 8, 9, 10, 11, 12, 13, 14])
                count <- Right (Msg.ByteCount $ Parser.convertLeWord32 msgBytes [15,16,17,18])
                validDataBytes <- Parser.parseValidDataBytes (Data.ByteString.drop fixedElementBytes msgBytes) count
                return $ callbacks.handleTWrite tag fid offset count validDataBytes
        in case parse of
            (Right value) -> value
            (Left err) -> callbacks.handleParseError err

fidOnlyCallback :: (Msg.Tag -> Msg.Fid -> a) -> (Parser.ParseError -> a) -> Msg.Tag -> Data.ByteString.ByteString -> a
fidOnlyCallback callback errCallback tag msgBytes
    | Data.ByteString.length msgBytes < Msg.messageTypeAndTagSize + 4 = errCallback Parser.OutOfBytes
    | otherwise = callback tag (Msg.Fid $ Parser.convertLeWord32 msgBytes [3, 4, 5, 6])

tWStatCallback :: RequestHandler a -> Msg.Tag -> Data.ByteString.ByteString -> a
tWStatCallback callbacks tag msgBytes
    | Data.ByteString.length msgBytes < Msg.messageTypeAndTagSize + 4 + 2 = callbacks.handleParseError Parser.OutOfBytes
    | otherwise =
        let parse = do
                let fixedElementBytes = Msg.messageTypeAndTagSize + 4 + 2
                    fid = Msg.Fid $ Parser.convertLeWord32 msgBytes [3, 4, 5, 6]
                    varArgByteCount = Msg.VarArgByteCount $ Parser.convertLeWord16 msgBytes [7, 8]
                let statBytes  = Data.ByteString.drop fixedElementBytes msgBytes
                stat <- Parser.parseStat statBytes
                return $ callbacks.handleTWstat tag fid varArgByteCount stat
        in case parse of
            (Right value) -> value
            (Left err) -> callbacks.handleParseError err
