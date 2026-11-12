{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Network.Plan9Hask.Parser where

import Data.Attoparsec.ByteString qualified
import Data.Bits
import Data.ByteString qualified
import Data.Serialize.Get qualified
import Data.Text qualified
import Data.Text.Encoding qualified
import Data.Word qualified
import Network.Plan9Hask.Msg qualified

maxIntAsWord32 :: Data.Word.Word32
maxIntAsWord32 = fromIntegral (maxBound :: Int)

msgSizeBytes :: Int
msgSizeBytes = 4

msgParser :: Data.Word.Word32 -> Data.Attoparsec.ByteString.Parser (Int, Data.ByteString.ByteString)
msgParser maxSize = do
  sizeBytes <- Data.Attoparsec.ByteString.take msgSizeBytes
  let parsedSize = Data.Serialize.Get.runGet Data.Serialize.Get.getWord32le sizeBytes
  size <- case parsedSize of
    (Right sizeWord) | sizeWord < 4 + 1 + 2 -> fail "size cannot fit the message type and tag"
    (Right sizeWord) | sizeWord > maxSize -> fail "size is larger than the maximum"
    (Right sizeWord) -> pure $ (fromIntegral sizeWord :: Int)
    (Left err) -> fail err
  msgBytes <- Data.Attoparsec.ByteString.take (size - msgSizeBytes)
  return (size, msgBytes)

runMsgParser :: Data.ByteString.ByteString -> Data.Attoparsec.ByteString.Result (Int, Data.ByteString.ByteString)
runMsgParser bytes = Data.Attoparsec.ByteString.parse (msgParser 8192) bytes

type Plan9Msg = Network.Plan9Hask.Msg.Value

data ParseError = OutOfBytes | ParseError | UnknownMsgType deriving (Show)

parseMsgBody :: (Int, Data.ByteString.ByteString) -> Either ParseError Plan9Msg
parseMsgBody (size, msgBytes)
  | Data.ByteString.length msgBytes < 1 + 2 = Left OutOfBytes
  | msgType <- (Data.ByteString.index msgBytes 0),
    tag <- parseTag msgBytes =
      handleMsgType msgType tag msgBytes

handleMsgType :: Data.Word.Word8 -> Data.Word.Word16 -> Data.ByteString.ByteString -> Either ParseError Plan9Msg
handleMsgType msgType tag msgBytes
  | msgType == Network.Plan9Hask.Msg.tVersion.getMsgType = parseTversion tag msgBytes
  | otherwise = Left UnknownMsgType

parseTversion tag msgBytes
  | Data.ByteString.length msgBytes < 1 + 2 + 4 = Left OutOfBytes
  | otherwise = case parsePlan9Str $ Data.ByteString.drop (1 + 2 + 4) msgBytes of
    Right versionStr -> Right $
        Network.Plan9Hask.Msg.Tversion
          { Network.Plan9Hask.Msg.mSize = convertLeWord32 msgBytes [3, 4, 5, 6],
            Network.Plan9Hask.Msg.version = versionStr,
            Network.Plan9Hask.Msg.tag = tag
          }
    Left err -> Left err

convertLeWord16 :: Data.ByteString.ByteString -> [Int] -> Data.Word.Word16
convertLeWord16 = convertLe

convertLeWord32 :: Data.ByteString.ByteString -> [Int] -> Data.Word.Word32
convertLeWord32 = convertLe

convertLe :: forall a. (Data.Bits.Bits a, Num a) => Data.ByteString.ByteString -> [Int] -> a
convertLe byteStr indices = go (zeroBits @a) 0 indices
  where
    go bits i (idx : rest) = go newBits (i + 8) rest
      where
        newBits = castByte `shift` i .|. bits
        castByte = fromIntegral @Data.Word.Word8 @a (Data.ByteString.index byteStr idx)
    go bits i [] = bits

parseTag :: Data.ByteString.ByteString -> Data.Word.Word16
parseTag msgBytes = firstByte .|. secondByte
  where
    firstByte :: Data.Word.Word16 = fromIntegral (Data.ByteString.index msgBytes 1)
    secondByte :: Data.Word.Word16 = fromIntegral (Data.ByteString.index msgBytes 2) `shift` 8

parsePlan9Str :: Data.ByteString.ByteString -> Either ParseError Data.Text.Text
parsePlan9Str byteStr
  | Data.ByteString.length byteStr < 2 = Left OutOfBytes
  | strSize <- convertLe byteStr [0, 1],
    strBytes <- Data.ByteString.take strSize (Data.ByteString.drop 2 byteStr) =
      if Data.ByteString.length strBytes < strSize
        then Left OutOfBytes
        else Right $ Data.Text.Encoding.decodeUtf8 strBytes
