{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Network.Plan9Hask.Builder where

import Data.ByteString qualified
import Data.ByteString.Builder qualified
import Data.Text qualified
import Data.Text.Encoding qualified
import Data.Word qualified
import Network.Plan9Hask.Msg (ByteCount (getByteCount))
import Network.Plan9Hask.Msg qualified as Msg

msgWithHeader :: Data.Word.Word32 -> Msg.MessageType -> Msg.Tag -> Data.ByteString.Builder.Builder
msgWithHeader size msgType tag =
  Data.ByteString.Builder.word32LE size
    <> (Data.ByteString.Builder.word8 $ msgType.getMsgType)
    <> (Data.ByteString.Builder.word16LE tag.getTag)

version9P2000 :: Data.Text.Text
version9P2000 = "9P2000"

versionByteString :: Data.ByteString.ByteString
versionByteString = Data.Text.Encoding.encodeUtf8 version9P2000

versionPlan9Str :: Data.ByteString.ByteString
versionPlan9Str = plan9Str version9P2000

tVersionMsgBuilder :: Msg.Tag -> Data.Word.Word32 -> Data.ByteString.Builder.Builder
tVersionMsgBuilder mTag mSize =
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

rErrorMsgBuilder :: Msg.Tag -> Data.Text.Text -> Data.ByteString.Builder.Builder
rErrorMsgBuilder mTag errorName =
  (msgWithHeader size Msg.rError mTag)
    <> Data.ByteString.Builder.byteString errorNamePlan9Str
  where
    errorNamePlan9Str = plan9Str errorName
    size :: Data.Word.Word32 = fromIntegral $ 4 + 1 + 2 + (Data.ByteString.length errorNamePlan9Str)

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
    <> (mconcat (map (\walkQid -> Data.ByteString.Builder.byteString walkQid.getQid) walkQids))
  where
    walkQidsLength = length walkQids
    size :: Data.Word.Word32 =
      fromIntegral $
        Msg.byteCountSize
          + Msg.messageTypeAndTagSize
          + Msg.arrayElementCountSize
          + (walkQidsLength * Msg.qidSize)

rOpenMsgBuilder :: Msg.Tag -> Msg.Qid -> Msg.ByteCount -> Data.ByteString.Builder.Builder
rOpenMsgBuilder mTag qid ioUnitByteCount =
  (msgWithHeader size Msg.rOpen mTag)
    <> qidBuilder qid
    <> Data.ByteString.Builder.word32LE ioUnitByteCount.getByteCount
  where
    size :: Data.Word.Word32 = fromIntegral $ Msg.byteCountSize + Msg.messageTypeAndTagSize + Msg.qidSize + Msg.byteCountSize

rReadMsgBuilder :: Msg.Tag -> Data.ByteString.ByteString -> Data.ByteString.Builder.Builder
rReadMsgBuilder mTag byteStr =
  (msgWithHeader size Msg.rRead mTag)
    <> Data.ByteString.Builder.word32LE byteStrLength
    <> Data.ByteString.Builder.byteString byteStr
  where
    byteStrLength = fromIntegral @Int @Data.Word.Word32 $ Data.ByteString.length byteStr
    size :: Data.Word.Word32 = fromIntegral $ 4 + Msg.messageTypeAndTagSize + Msg.byteCountSize + (Data.ByteString.length byteStr)

rStatMsgBuilder :: Msg.Tag -> Msg.Stat -> Data.ByteString.Builder.Builder
rStatMsgBuilder mTag stat =
  (msgWithHeader size Msg.rStat mTag)
    <> Data.ByteString.Builder.word16LE (fromIntegral $ statByteStrLength) -- Possible overflow
    <> Data.ByteString.Builder.byteString statByteStr
  where
    statByteStrLength = Data.ByteString.length statByteStr
    statByteStr = statByteString stat
    size :: Data.Word.Word32 = fromIntegral $ Msg.byteCountSize + Msg.messageTypeAndTagSize + 2 + statByteStrLength

rClunkMsgBuilder :: Msg.Tag -> Data.ByteString.Builder.Builder
rClunkMsgBuilder mTag =
  (msgWithHeader size Msg.rClunk mTag)
  where
    size :: Data.Word.Word32 = fromIntegral $ 4 + Msg.messageTypeAndTagSize

qidBuilder :: Msg.Qid -> Data.ByteString.Builder.Builder
qidBuilder aQid = Data.ByteString.Builder.byteString aQid.getQid

statByteString :: Msg.Stat -> Data.ByteString.ByteString
statByteString stat = Data.ByteString.toStrict $ Data.ByteString.Builder.toLazyByteString builder
  where
    builder =
      (Data.ByteString.Builder.word16LE (fromIntegral size)) -- Possible overflow
        <> (Data.ByteString.Builder.word16LE stat.statType)
        <> (Data.ByteString.Builder.word32LE stat.statDev)
        <> (Data.ByteString.Builder.byteString stat.qid.getQid)
        <> (Data.ByteString.Builder.word32LE stat.mode.getPermissions)
        <> (Data.ByteString.Builder.word32LE stat.atime.getEpochSeconds)
        <> (Data.ByteString.Builder.word32LE stat.mtime.getEpochSeconds)
        <> (Data.ByteString.Builder.word64LE stat.length.getOffset)
        <> (Data.ByteString.Builder.byteString fileNamePlan9Str)
        <> (Data.ByteString.Builder.byteString userIdPlan9Str)
        <> (Data.ByteString.Builder.byteString groupIdPlan9Str)
        <> (Data.ByteString.Builder.byteString modifierUserIdPlan9Str)
    size =
      Msg.varArgByteCountSize
        + Msg.statTypeSize
        + Msg.statDevSize
        + Msg.qidSize
        + Msg.permissionsSize
        + (Data.ByteString.length fileNamePlan9Str)
        + (Data.ByteString.length userIdPlan9Str)
        + (Data.ByteString.length groupIdPlan9Str)
        + (Data.ByteString.length modifierUserIdPlan9Str)
    fileNamePlan9Str = plan9Str stat.fileName
    userIdPlan9Str = plan9Str stat.uid
    groupIdPlan9Str = plan9Str stat.gid
    modifierUserIdPlan9Str = plan9Str stat.muid

plan9Str :: Data.Text.Text -> Data.ByteString.ByteString
plan9Str str =
  Data.ByteString.toStrict $ Data.ByteString.Builder.toLazyByteString $ Data.ByteString.Builder.word16LE strLength <> Data.ByteString.Builder.byteString strByteString
  where
    strByteString = Data.Text.Encoding.encodeUtf8 str
    strLength :: Data.Word.Word16 = (fromIntegral (Data.ByteString.length strByteString)) -- Possible overflow
