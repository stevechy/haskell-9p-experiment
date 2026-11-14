{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Network.Plan9Hask.RecordParser where

import Data.ByteString qualified
import Data.Word qualified
import Network.Plan9Hask.Msg qualified as Msg
import Network.Plan9Hask.Parser qualified as Parser

type Plan9Msg = Msg.Value

parseMsgBody :: (Int, Data.ByteString.ByteString) -> Either Parser.ParseError Plan9Msg
parseMsgBody (_size, msgBytes)
  | Data.ByteString.length msgBytes < 1 + 2 = Left Parser.OutOfBytes
  | msgType <- (Data.ByteString.index msgBytes 0),
    tag <- Parser.parseTag msgBytes =
      handleMsgType msgType tag msgBytes

handleMsgType :: Data.Word.Word8 -> Msg.Tag -> Data.ByteString.ByteString -> Either Parser.ParseError Plan9Msg
handleMsgType msgType tag msgBytes
  | msgType == Msg.tVersion.getMsgType = parseTversion tag msgBytes
  | otherwise = Left Parser.UnknownMsgType

parseTversion :: Msg.Tag -> Data.ByteString.ByteString -> Either Parser.ParseError Plan9Msg
parseTversion tag msgBytes
  | Data.ByteString.length msgBytes < 1 + 2 + 4 = Left Parser.OutOfBytes
  | otherwise = case Parser.parsePlan9Str $ Data.ByteString.drop (1 + 2 + 4) msgBytes of
      Right versionStr ->
        Right $
          Msg.Tversion
            { Msg.mSize = Parser.convertLeWord32 msgBytes [3, 4, 5, 6],
              Msg.version = versionStr,
              Msg.tag = tag.getTag
            }
      Left err -> Left err
