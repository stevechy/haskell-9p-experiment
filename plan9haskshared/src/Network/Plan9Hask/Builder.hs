{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.Plan9Hask.Builder where

import Data.Word qualified
import Data.ByteString qualified
import Data.ByteString.Builder qualified
import Network.Plan9Hask.Msg qualified as Msg
import Data.Text qualified
import Data.Text.Encoding qualified

msgWithHeader :: Data.Word.Word32 -> Msg.MessageType -> Msg.Tag -> Data.ByteString.Builder.Builder
msgWithHeader size msgType tag =
  Data.ByteString.Builder.word32LE size
    <> (Data.ByteString.Builder.word8 $ msgType.getMsgType)
    <> Data.ByteString.Builder.word16LE tag.getTag

version9P2000 :: Data.Text.Text
version9P2000 = "9P2000"

versionByteString :: Data.ByteString.ByteString
versionByteString = Data.Text.Encoding.encodeUtf8 version9P2000

versionPlan9Str :: Data.ByteString.ByteString
versionPlan9Str = plan9Str version9P2000

tVersionMsgBuilder :: Msg.Tag -> Data.Word.Word32 -> Data.ByteString.Builder.Builder
tVersionMsgBuilder mTag mSize  =
  (msgWithHeader size Msg.tVersion mTag)
    <> Data.ByteString.Builder.word32LE mSize
    <> Data.ByteString.Builder.byteString versionPlan9Str
  where
    size :: Data.Word.Word32 = fromIntegral $ 4 + 1 + 2 + 4 + (Data.ByteString.length versionPlan9Str)

rVersionMsgBuilder :: Msg.Tag -> Data.Word.Word32 -> Data.ByteString.Builder.Builder
rVersionMsgBuilder mTag mSize =
  (msgWithHeader size Msg.rVersion mTag)
      <> Data.ByteString.Builder.word32LE mSize
      <> Data.ByteString.Builder.byteString versionPlan9Str
        where
          size :: Data.Word.Word32 = fromIntegral $ 4 + 1 + 2 + 4 + (Data.ByteString.length versionPlan9Str)

rAuthMsgBuilder :: Msg.Tag -> Msg.Qid -> Data.ByteString.Builder.Builder
rAuthMsgBuilder mTag aQid =
    (msgWithHeader size Msg.rAuth mTag)
        <> qidBuilder aQid
            where
            size :: Data.Word.Word32 = fromIntegral $ Msg.byteCountSize + Msg.messageTypeAndTagSize + Msg.qidSize


rAttachMsgBuilder :: Msg.Tag -> Msg.Qid -> Data.ByteString.Builder.Builder
rAttachMsgBuilder mTag qid =
    (msgWithHeader size Msg.rAttach mTag)
        <> qidBuilder qid
            where
            size :: Data.Word.Word32 = fromIntegral $ Msg.byteCountSize + Msg.messageTypeAndTagSize + Msg.qidSize

rWalkMsgBuilder :: Msg.Tag -> [Msg.Qid] -> Data.ByteString.Builder.Builder
rWalkMsgBuilder tag walkQids =
    (msgWithHeader size Msg.rWalk tag)
        <> (Data.ByteString.Builder.word16LE (fromIntegral walkQidsLength)) -- Possible overflow
        <> (mconcat (map (\ walkQid -> Data.ByteString.Builder.byteString walkQid.getQid) walkQids))
            where
            walkQidsLength = length walkQids
            size :: Data.Word.Word32 = fromIntegral $ Msg.byteCountSize +
                Msg.messageTypeAndTagSize +
                Msg.arrayElementCountSize +
                (walkQidsLength * Msg.qidSize)

qidBuilder :: Msg.Qid -> Data.ByteString.Builder.Builder
qidBuilder aQid = Data.ByteString.Builder.byteString aQid.getQid

plan9Str :: Data.Text.Text -> Data.ByteString.ByteString
plan9Str str =
    Data.ByteString.toStrict $ Data.ByteString.Builder.toLazyByteString $ Data.ByteString.Builder.word16LE strLength <> Data.ByteString.Builder.byteString strByteString
    where
    strByteString = Data.Text.Encoding.encodeUtf8 str
    strLength :: Data.Word.Word16 = (fromIntegral (Data.ByteString.length strByteString)) -- Possible overflow
