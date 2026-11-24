module Network.Plan9Hask.Error where

import Network.Plan9Hask.Msg qualified as Msg

-- Update to be mappable to errno
-- https://github.com/torvalds/linux/blob/master/include/uapi/asm-generic/errno-base.h
data Error
  = FidInUse
  | FidInUseWithFid Msg.Fid
  | FidAlreadyOpen
  | FidNotAttached
  | FidNotOpen
  | FidUnexpectedReadError
  | NotImplemented
  | NoPathEntry
  | NotADir
  | IsADir
  deriving (Show, Eq)
