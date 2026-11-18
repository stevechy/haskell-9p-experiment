{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoFieldSelectors #-}

module Network.Plan9Hask.MinimalHandler where

import Control.Concurrent.Async qualified
import Control.Exception qualified
import Control.Monad qualified
import Data.ByteString qualified
import Data.ByteString.Builder qualified
import Data.IORef qualified
import Data.Int
import Data.List.NonEmpty qualified
import Data.Map qualified
import Data.Text qualified
import Data.Text.IO qualified
import Data.Word qualified
import Network.Plan9Hask.Builder qualified
import Network.Plan9Hask.HandlerParser (Plan9Callback (Plan9Callback))
import Network.Plan9Hask.HandlerParser qualified
import Network.Plan9Hask.Msg qualified as Msg
import Network.Socket qualified
import Network.Socket.ByteString qualified
import Network.Socket.ByteString.Lazy qualified

data FidState
  = AttachedFid
      { path :: [Data.Text.Text]
      }
  | OpenFid
      { path :: [Data.Text.Text],
        mode :: Msg.OpenMode
      }
  | ClunkedFid {}

data HandlerState = HandlerState
  { fids :: Data.Map.Map Msg.Fid [FidState]
  }

data Error = FidInUse | FidAlreadyOpen | FidNotAttached deriving (Show, Eq)

fidInUse :: FidState -> Bool
fidInUse (AttachedFid {}) = True
fidInUse (OpenFid {}) = True
fidInUse (ClunkedFid {}) = False

attachFid :: Data.IORef.IORef HandlerState -> Msg.Fid -> IO (Either Error FidState)
attachFid fidState fid = do
  Data.IORef.atomicModifyIORef' fidState $ \currentHandler ->
    case (Data.Map.lookup fid currentHandler.fids) of
      (Just fidList) | (any fidInUse fidList) -> (currentHandler, Left FidInUse)
      (result) ->
        ( currentHandler {fids = Data.Map.insert fid (attachedFid : fidList) currentHandler.fids},
          Right attachedFid
        )
        where
          fidList = maybe [] id result
          attachedFid = AttachedFid {path = []}

openFid :: Data.IORef.IORef HandlerState -> Msg.Fid -> Msg.OpenMode -> IO (Either Error FidState)
openFid fidState fid openMode = do
  Data.IORef.atomicModifyIORef' fidState $ \currentHandler ->
    case (Data.Map.lookup fid currentHandler.fids) of
      (Just (currentFid@AttachedFid {} : rest)) ->
        ( currentHandler {fids = Data.Map.insert fid (openedFid : rest) currentHandler.fids},
          Right openedFid
        )
        where
          openedFid = OpenFid {path = currentFid.path, mode = openMode}
      (Just (OpenFid {} : _)) -> (currentHandler, Left FidAlreadyOpen)
      (Just (ClunkedFid {} : _)) -> (currentHandler, Left FidNotAttached)
      (Just []) -> (currentHandler, Left FidNotAttached)
      (Nothing) -> (currentHandler, Left FidNotAttached)

newHandler :: Network.Socket.Socket -> IO (Network.Plan9Hask.HandlerParser.Plan9Callback (IO ()))
newHandler sock = do
  fidState <- Data.IORef.newIORef $ HandlerState {fids = Data.Map.empty}
  let rootDirQidPath :: [Data.Word.Word8]
      rootDirQidPath = [0, 0, 0, 0, 0, 0, 0, 7]
      rootQid = Msg.Qid $ Data.ByteString.pack $ concat [[Msg.dirQidType], [0, 0, 0, 0], rootDirQidPath]
      sendResponse bStrBuilder = Network.Socket.ByteString.Lazy.sendAll sock (Data.ByteString.Builder.toLazyByteString bStrBuilder)
      handleTVersion tag mSize version = do
        Data.Text.IO.putStrLn $
          Data.Text.concat
            [ "tVersion ",
              (Data.Text.pack $ show tag),
              " m ",
              (Data.Text.pack $ show mSize),
              " v ",
              (Data.Text.pack $ show version)
            ]
        sendResponse $ Network.Plan9Hask.Builder.rVersionMsgBuilder tag (min mSize 8192)
      handleTAuth tag authFid username attachName = Data.Text.IO.putStrLn $ Data.Text.concat ["tAuth"]
      handleTFlush tag oldTag = Data.Text.IO.putStrLn $ Data.Text.concat ["tFlush"]
      handleTAttach tag fid authFid username attachName = do
        Data.Text.IO.putStrLn $
          Data.Text.concat
            [ "tAttach",
              (Data.Text.pack $ show tag),
              " m "
            ]
        attachResult <- attachFid fidState fid
        case attachResult of
          (Left err) -> sendResponse $ Network.Plan9Hask.Builder.rErrorMsgBuilder tag $ Data.Text.show err
          (Right _) -> sendResponse $ Network.Plan9Hask.Builder.rAttachMsgBuilder tag rootQid
      handleTWalk :: Msg.Tag -> Msg.Fid -> Msg.Fid -> Msg.ArrayElementCount -> [Data.Text.Text] -> IO ()
      handleTWalk tag fid newFid walkNameCount walkNames = do
        Data.Text.IO.putStrLn $ Data.Text.concat ["tWalk"]
        if walkNameCount.getArrayElementCount == 0
          then sendResponse $ Network.Plan9Hask.Builder.rWalkMsgBuilder tag []
          else sendResponse $ Network.Plan9Hask.Builder.rWalkMsgBuilder tag []
      handleTOpen tag fid openMode = Data.Text.IO.putStrLn $ Data.Text.concat ["tOpen"]
      handleTCreate tag fid fileName permissions mode = Data.Text.IO.putStrLn $ Data.Text.concat ["tCreate"]
      handleTRead tag fid offset readByteCount = Data.Text.IO.putStrLn $ Data.Text.concat ["tRead"]
      handleTWrite tag fid offset writeByteCount writeData = Data.Text.IO.putStrLn $ Data.Text.concat ["tWrite"]
      handleTClunk tag fid = Data.Text.IO.putStrLn $ Data.Text.concat ["tClunk"]
      handleTRemove tag fid = Data.Text.IO.putStrLn $ Data.Text.concat ["tRemove"]
      handleTStat tag fid = Data.Text.IO.putStrLn $ Data.Text.concat ["tStat"]
      handleTWstat tag fid statByteCount stat = Data.Text.IO.putStrLn $ Data.Text.concat ["tWStat"]
      handleParseError err = Data.Text.IO.putStrLn $ Data.Text.concat ["Parse error ", Data.Text.pack $ show err]
      callback = Network.Plan9Hask.HandlerParser.Plan9Callback {..}
  return callback
