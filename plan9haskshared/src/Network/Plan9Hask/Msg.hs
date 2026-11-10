module Network.Plan9Hask.Msg where

import Data.Word qualified

newtype MessageType = MessageType Data.Word.Word8

tversion = MessageType 100

rversion = MessageType 101

tauth = MessageType 102

rauth = MessageType 103

tattach = MessageType 104

rattach = MessageType 105

terror = MessageType 106

rerror = MessageType 107

tflush = MessageType 108

rflush = MessageType 109

twalk = MessageType 110

rwalk = MessageType 111

topen = MessageType 112

ropen = MessageType 113

tcreate = MessageType 114

rcreate = MessageType 115

tread = MessageType 116

rread = MessageType 117

twrite = MessageType 118

rwrite = MessageType 119

tclunk = MessageType 120

rclunk = MessageType 121

tremove = MessageType 122

rremove = MessageType 123

tstat = MessageType 124

rstat = MessageType 125

twstat = MessageType 126

rwstat = MessageType 127

tmax = MessageType 128

topenfd = MessageType 98

ropenfd = MessageType 99
