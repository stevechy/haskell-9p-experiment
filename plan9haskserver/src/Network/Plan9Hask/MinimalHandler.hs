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
import Network.Plan9Hask.HandlerParser qualified
import Network.Plan9Hask.Msg qualified as Msg
import Network.Socket qualified
import Network.Socket.ByteString.Lazy qualified
import Network.Plan9Hask.Error

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
  { fids :: Data.Map.Map Msg.Fid [FidState],
    rootDirQidPath :: [Data.Word.Word8],
    rootDirQidVersion :: [Data.Word.Word8]
  }

fidInUse :: FidState -> Bool
fidInUse (AttachedFid {}) = True
fidInUse (OpenFid {}) = True
fidInUse (ClunkedFid {}) = False

resolve :: HandlerState -> [Data.Text.Text] -> Maybe Msg.Qid
resolve handlerState [] = Just $ Msg.Qid $ Data.ByteString.pack $ concat [[Msg.dirQidType], handlerState.rootDirQidVersion, handlerState.rootDirQidPath]
resolve _ _ = Nothing

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

walkFid :: Data.IORef.IORef HandlerState -> Msg.Fid -> [Data.Text.Text] -> Msg.Fid -> IO (Either Error FidState)
walkFid fidState fid walkNames newFid = do
  Data.IORef.atomicModifyIORef' fidState $ \currentHandler ->
    case (Data.Map.lookup fid currentHandler.fids) of
      (Just (currentFid@AttachedFid {} : _)) ->
        case (Data.Map.lookup newFid currentHandler.fids) of
          (Just (AttachedFid {} : fidList)) | newFid == fid ->
            case resolve currentHandler resultPath of
              Just _qid -> (currentHandler {fids = Data.Map.insert newFid (attachedFid : fidList) currentHandler.fids}, Right attachedFid)
              Nothing -> (currentHandler, Left PathDoesNotExist)
            where
              resultPath = (currentFid.path ++ walkNames)
              attachedFid = AttachedFid {path = resultPath}
          (Just fidList) | (any fidInUse fidList) -> (currentHandler, Left FidInUse)
          (result) ->
            case resolve currentHandler resultPath of
              Just _qid -> (currentHandler {fids = Data.Map.insert newFid (attachedFid : fidList) currentHandler.fids}, Right attachedFid)
              Nothing -> (currentHandler, Left PathDoesNotExist)
            where
              resultPath = (currentFid.path ++ walkNames)
              fidList = maybe [] id result
              attachedFid = AttachedFid {path = resultPath}
      (Just (OpenFid {} : _)) -> (currentHandler, Left FidAlreadyOpen)
      (Just (ClunkedFid {} : _)) -> (currentHandler, Left FidNotAttached)
      (Just []) -> (currentHandler, Left FidNotAttached)
      (Nothing) -> (currentHandler, Left FidNotAttached)

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

readFid :: Data.IORef.IORef HandlerState -> Msg.Fid -> IO (Either Error FidState)
readFid fidState fid = do
  Data.IORef.atomicModifyIORef' fidState $ \currentHandler ->
    case (Data.Map.lookup fid currentHandler.fids) of
      (Just (currentFid@OpenFid {} : _)) ->
        (currentHandler, Right currentFid)
      (Just (AttachedFid {} : _)) -> (currentHandler, Left FidNotOpen)
      (Just (ClunkedFid {} : _)) -> (currentHandler, Left FidNotAttached)
      (Just []) -> (currentHandler, Left FidNotAttached)
      (Nothing) -> (currentHandler, Left FidNotAttached)

clunkFid :: Data.IORef.IORef HandlerState -> Msg.Fid -> IO (Either Error FidState)
clunkFid fidState fid = do
  Data.IORef.atomicModifyIORef' fidState $ \currentHandler ->
    case (Data.Map.lookup fid currentHandler.fids) of
      (Just (AttachedFid {} : rest)) ->
        ( currentHandler {fids = Data.Map.insert fid (clunkedFid : rest) currentHandler.fids},
          Right clunkedFid
        )
        where
          clunkedFid = ClunkedFid {}
      (Just (OpenFid {} : rest)) ->
        ( currentHandler {fids = Data.Map.insert fid (clunkedFid : rest) currentHandler.fids},
          Right clunkedFid
        )
        where
          clunkedFid = ClunkedFid {}
      (Just (clunkedFid@ClunkedFid {} : rest)) ->
        ( currentHandler {fids = Data.Map.insert fid (clunkedFid : rest) currentHandler.fids},
          Right clunkedFid
        )
      (Just []) -> (currentHandler, Left FidNotAttached)
      (Nothing) -> (currentHandler, Left FidNotAttached)

rootDirStat :: Msg.Qid -> Msg.Stat
rootDirStat qid = Msg.Stat {
    statType = 0,
    statDev = 0,
    qid = qid,
    mode = Msg.directoryPermission Msg.worldReadblePermission,
    fileName = "/",
    length = Msg.Offset $ 0,
    atime = Msg.EpochSeconds $ 0,
    mtime = Msg.EpochSeconds $ 0,
    uid = "root",
    gid = "root",
    muid = "root"
}

newHandler :: Network.Socket.Socket -> IO (Network.Plan9Hask.HandlerParser.Plan9Callback (IO ()))
newHandler sock = do
  let rootDirQidPath :: [Data.Word.Word8]
      rootDirQidPath = [7, 0, 0, 0, 0, 0, 0, 0]
      rootDirQidVersion :: [Data.Word.Word8]
      rootDirQidVersion = [1, 0, 0, 0]
      rootQid = Msg.Qid $ Data.ByteString.pack $ concat [[Msg.dirQidType], rootDirQidVersion, rootDirQidPath]
  fidState <- Data.IORef.newIORef $ HandlerState {fids = Data.Map.empty, rootDirQidPath = rootDirQidPath, rootDirQidVersion = rootDirQidVersion}
  let defaultIoUnitByteCount = Msg.ByteCount $ 4096
      sendResponse bStrBuilder = Network.Socket.ByteString.Lazy.sendAll sock (Data.ByteString.Builder.toLazyByteString bStrBuilder)
      handleTVersion tag mSize version = do
        sendResponse $ Network.Plan9Hask.Builder.rVersionMsgBuilder tag (min mSize 8192)
      handleTAuth tag authFid username attachName =
        Data.Text.IO.putStrLn $
          Data.Text.concat
            [ "tAuth ",
              (Data.Text.pack $ show tag)
            ]
      handleTFlush tag oldTag =
        Data.Text.IO.putStrLn $
          Data.Text.concat
            [ "tFlush ",
              (Data.Text.pack $ show tag)
            ]
      handleTAttach tag fid authFid username attachName = do
        attachResult <- attachFid fidState fid
        case attachResult of
          (Left err) -> sendResponse $ Network.Plan9Hask.Builder.rErrorMsgBuilder tag $ Data.Text.show err
          (Right _) -> sendResponse $ Network.Plan9Hask.Builder.rAttachMsgBuilder tag rootQid
      handleTWalk :: Msg.Tag -> Msg.Fid -> Msg.Fid -> Msg.ArrayElementCount -> [Data.Text.Text] -> IO ()
      handleTWalk tag fid newFid _walkNameCount walkNames = do
        walkResult <- walkFid fidState fid walkNames newFid
        case walkResult of
          (Right attachedNewFid) -> sendResponse $ Network.Plan9Hask.Builder.rWalkMsgBuilder tag []
          (Left PathDoesNotExist) -> sendResponse $ Network.Plan9Hask.Builder.rWalkMsgBuilder tag []
          (Left err) -> sendResponse $ Network.Plan9Hask.Builder.rErrorMsgBuilder tag $ Data.Text.show err
      handleTOpen tag fid openMode = do
        openResult <- openFid fidState fid openMode
        case openResult of
          (Left err) -> sendResponse $ Network.Plan9Hask.Builder.rErrorMsgBuilder tag $ Data.Text.show err
          (Right _) -> sendResponse $ Network.Plan9Hask.Builder.rOpenMsgBuilder tag rootQid defaultIoUnitByteCount
      handleTCreate tag fid fileName permissions mode = Data.Text.IO.putStrLn $ Data.Text.concat ["tCreate"]
      handleTRead tag fid offset readByteCount = do
        readOkResult <- readFid fidState fid
        case readOkResult of
          (Left err) -> sendResponse $ Network.Plan9Hask.Builder.rErrorMsgBuilder tag $ Data.Text.show err
          (Right openFid@OpenFid {}) | openFid.path == [] -> sendResponse $ Network.Plan9Hask.Builder.rReadMsgBuilder tag Data.ByteString.empty -- root dir read
          (Right _) -> sendResponse $ Network.Plan9Hask.Builder.rErrorMsgBuilder tag $ Data.Text.show FidUnexpectedReadError -- root dir read
      handleTWrite tag fid offset writeByteCount writeData = Data.Text.IO.putStrLn $ Data.Text.concat ["tWrite"]
      handleTClunk tag fid = do
        clunkResult <- clunkFid fidState fid
        case clunkResult of
            (Left err) -> sendResponse $ Network.Plan9Hask.Builder.rErrorMsgBuilder tag $ Data.Text.show err
            (Right _) -> sendResponse $ Network.Plan9Hask.Builder.rClunkMsgBuilder tag
      handleTRemove tag fid = Data.Text.IO.putStrLn $ Data.Text.concat ["tRemove"]
      handleTStat tag fid = do
        readFidState <- Data.IORef.readIORef fidState
        case Data.Map.lookup fid readFidState.fids of
            Just (attachedFid@AttachedFid{} : _) | attachedFid.path == [] -> sendResponse $ Network.Plan9Hask.Builder.rStatMsgBuilder tag (rootDirStat rootQid)
            Just (attachedFid@AttachedFid{} : _) -> sendResponse $ Network.Plan9Hask.Builder.rErrorMsgBuilder tag $ Data.Text.show PathDoesNotExist
            _ -> sendResponse $ Network.Plan9Hask.Builder.rErrorMsgBuilder tag $ Data.Text.show FidNotAttached
      handleTWstat tag fid statByteCount stat = Data.Text.IO.putStrLn $ Data.Text.concat ["tWStat"]
      handleParseError err = Data.Text.IO.putStrLn $ Data.Text.concat ["Parse error ", Data.Text.pack $ show err]
      callback = Network.Plan9Hask.HandlerParser.Plan9Callback {..}
  return callback
