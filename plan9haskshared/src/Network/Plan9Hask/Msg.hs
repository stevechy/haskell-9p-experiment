{-# LANGUAGE OverloadedRecordDot #-}

module Network.Plan9Hask.Msg where

import Data.Word qualified
import Data.Text qualified

newtype MessageType = MessageType{ getMsgType :: Data.Word.Word8 }

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


data Value = Tversion { tag:: Data.Word.Word16, mSize :: Data.Word.Word32, version:: Data.Text.Text } deriving Show
