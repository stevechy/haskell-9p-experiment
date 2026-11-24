{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoFieldSelectors #-}

module Network.Plan9Hask.ShowHandler where

import Control.Concurrent.Async qualified
import Control.Exception qualified
import Control.Monad qualified
import Data.ByteString qualified
import Data.ByteString.Builder qualified
import Data.IORef qualified
import Data.Int
import Data.List.NonEmpty qualified
import Data.List qualified
import Data.Map qualified
import Data.Text qualified
import Data.Text.IO qualified
import Data.Word qualified
import Network.Plan9Hask.Builder qualified
import Network.Plan9Hask.Error
import Network.Plan9Hask.HandlerParser qualified
import Network.Plan9Hask.Msg qualified as Msg
import Network.Socket qualified
import Network.Socket.ByteString.Lazy qualified

handler :: Network.Plan9Hask.HandlerParser.Plan9Callback Data.Text.Text
handler =
  let buildText = Data.Text.concat . (Data.List.intersperse " ")
      handleTVersion tag mSize version =
        buildText
          [ "tVersion ",
            (Data.Text.show $ tag),
            " m ",
            (Data.Text.show $ mSize),
            " v ",
            (Data.Text.show $ version)
          ]
      handleTAuth tag authFid username attachName =
        buildText
          [ "tAuth ",
            (Data.Text.show $ tag),
            (Data.Text.show $ authFid),
            (Data.Text.show $ username),
            (Data.Text.show $ attachName)
          ]
      handleTFlush tag oldTag =
        buildText
          [ "tFlush ",
            (Data.Text.show $ tag),
            "oldTag",
            (Data.Text.show $ oldTag)
          ]
      handleTAttach tag fid authFid username attachName =
        buildText
          [ "tAttach ",
            (Data.Text.show $ tag),
            (Data.Text.show $ fid),
            (Data.Text.show $ authFid),
            (Data.Text.show $ username),
            (Data.Text.show $ attachName)
          ]
      handleTWalk tag fid newFid walkNameCount walkNames =
        buildText
          [ "tWalk ",
            (Data.Text.show tag),
            (Data.Text.show fid),
            (Data.Text.show walkNameCount),
            (Data.Text.show walkNames),
            "newFid",
            (Data.Text.show newFid)
          ]
      handleTOpen tag fid openMode =
        buildText
          [ "tOpen ",
            (Data.Text.show $ tag),
            (Data.Text.show $ fid),
            (Data.Text.show $ openMode)
          ]
      handleTCreate tag fid fileName permissions mode =
        buildText
          [ "tCreate ",
            (Data.Text.show $ tag),
            (Data.Text.show $ fid),
            (Data.Text.show $ fileName),
            (Data.Text.show $ permissions),
            (Data.Text.show $ mode)
          ]
      handleTRead tag fid offset readByteCount =
        buildText
          [ "tRead ",
            (Data.Text.show $ tag),
            (Data.Text.show $ fid),
            (Data.Text.show $ offset),
            (Data.Text.show $ readByteCount)
          ]
      handleTWrite tag fid offset writeByteCount writeData =
        buildText
          [ "tWrite ",
            (Data.Text.show $ tag),
            (Data.Text.show $ fid),
            (Data.Text.show $ offset),
            (Data.Text.show $ writeByteCount)
          ]
      handleTClunk tag fid =
        buildText
          [ "tClunk ",
            (Data.Text.show $ tag),
            (Data.Text.show $ fid)
          ]
      handleTRemove tag fid =
        buildText
          [ "tRemove ",
            (Data.Text.show $ tag),
            (Data.Text.show $ fid)
          ]
      handleTStat tag fid =
        buildText
          [ "tStat ",
            (Data.Text.show $ tag),
            (Data.Text.show $ fid)
          ]
      handleTWstat tag fid statByteCount stat =
        buildText
          [ "tWStat ",
            (Data.Text.show $ tag),
            (Data.Text.show $ fid),
            (Data.Text.show $ statByteCount),
            (Data.Text.show $ stat)
          ]
      handleParseError err = Data.Text.concat ["Parse error ", Data.Text.show $ err]
   in Network.Plan9Hask.HandlerParser.Plan9Callback {..}
