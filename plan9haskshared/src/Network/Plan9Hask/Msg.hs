{-# LANGUAGE OverloadedRecordDot #-}

module Network.Plan9Hask.Msg where

import Data.Text qualified
import Data.Word qualified
import Data.ByteString qualified

newtype MessageType = MessageType {getMsgType :: Data.Word.Word8}

tVersion = MessageType 100

rVersion = MessageType 101

tAuth = MessageType 102

rAuth = MessageType 103

tAttach = MessageType 104

rAttach = MessageType 105

tError = MessageType 106

rError = MessageType 107

tFlush = MessageType 108

rFlush = MessageType 109

tWalk = MessageType 110

rWalk = MessageType 111

tOpen = MessageType 112

rOpen = MessageType 113

tCreate = MessageType 114

rCreate = MessageType 115

tRead = MessageType 116

rRead = MessageType 117

tWrite = MessageType 118

rWrite = MessageType 119

tClunk = MessageType 120

rClunk = MessageType 121

tRemove = MessageType 122

rRemove = MessageType 123

tStat = MessageType 124

rStat = MessageType 125

tWstat = MessageType 126

rWstat = MessageType 127

tMax = MessageType 128

tOpenfd = MessageType 98

rOpenfd = MessageType 99

newtype Tag = Tag {getTag :: Data.Word.Word16} deriving Show

newtype Fid = Fid {getFid :: Data.Word.Word32} deriving Show
newtype AuthFid = AuthFid {getAuthFid :: Data.Word.Word32} deriving Show
newtype Username = Username {getUsername :: Data.Text.Text} deriving Show
newtype AttachName = AttachName {getAttachName :: Data.Text.Text} deriving Show

newtype ArrayElementCount = ArrayElementCount { getArrayElementCount :: Data.Word.Word16 } deriving Show

newtype ByteCount = ByteCount { getByteCount :: Data.Word.Word32 } deriving Show
byteCountSize :: Int
byteCountSize = 4

newtype VarArgByteCount = VarArgByteCount { getVarArgByteCount:: Data.Word.Word16 } deriving Show
varArgByteCountSize :: Int
varArgByteCountSize = 2

newtype EpochSeconds = EpochSeconds { getEpochSeconds :: Data.Word.Word32 } deriving Show
epochSecondsSize :: Int
epochSecondsSize = 4

newtype Qid = Qid { getQid :: Data.ByteString.ByteString } deriving Show
qidSize :: Int
qidSize = 13

newtype OpenMode = OpenMode { getOpenMode :: Data.Word.Word8 } deriving Show
newtype CreateMode = CreateMode { getCreateMode :: Data.Word.Word8 } deriving Show
newtype Permissions = Permissions {getPermissions:: Data.Word.Word32 } deriving Show
permissionsSize :: Int
permissionsSize = 4

newtype Offset = Offset { getOffset:: Data.Word.Word64 } deriving Show
offsetSize :: Int
offsetSize = 8

data Stat = Stat {
    size :: VarArgByteCount,
    statType :: Data.Word.Word16,
    statDev :: Data.Word.Word32,
    qid :: Qid,
    mode :: Permissions,
    atime :: EpochSeconds,
    mtime :: EpochSeconds,
    length :: Offset,
    fileName :: Data.Text.Text,
    uid :: Data.Text.Text,
    gid :: Data.Text.Text,
    muid :: Data.Text.Text
}

statTypeSize :: Int
statTypeSize = 2
statDevSize :: Int
statDevSize = 4

messageTypeSize :: Int
messageTypeSize = 1

messageTagSize :: Int
messageTagSize = 2

messageTypeAndTagSize :: Int
messageTypeAndTagSize = 1 + 2

data Value = Tversion {tag :: Data.Word.Word16, mSize :: Data.Word.Word32, version :: Data.Text.Text} deriving (Show)
