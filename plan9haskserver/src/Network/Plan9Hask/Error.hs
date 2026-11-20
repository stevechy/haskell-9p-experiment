module Network.Plan9Hask.Error where

-- Update to be mappable to errno
-- https://github.com/torvalds/linux/blob/master/include/uapi/asm-generic/errno-base.h
data Error = FidInUse | FidAlreadyOpen | FidNotAttached | FidNotOpen | FidUnexpectedReadError | NoPathEntry deriving (Show, Eq)
