{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoFieldSelectors #-}

module Network.Plan9Hask.MapFsHandler where

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
import Network.Plan9Hask.Error
import Network.Plan9Hask.HandlerParser qualified
import Network.Plan9Hask.Msg qualified as Msg
import Network.Socket qualified
import Network.Socket.ByteString.Lazy qualified

data FidState
  = AttachedFid
      { walkedQidPath :: [WalkedQidPath]
      }
  | OpenFid
      { walkedQidPath :: [WalkedQidPath],
        mode :: Msg.OpenMode
      }
  | ClunkedFid {}

data WalkedQidPath = WalkedQidPath {path :: Data.Text.Text, qidPath :: Msg.QidPath} deriving (Show, Eq, Ord)

data QidPath = RootQidPath | QidPath {qid :: Msg.QidPath} deriving (Show, Eq, Ord)

data NodeStat = NodeStat
  { mode :: Msg.Permissions,
    atime :: Msg.EpochSeconds,
    mtime :: Msg.EpochSeconds,
    fileName :: Data.Text.Text,
    uid :: Data.Text.Text,
    gid :: Data.Text.Text,
    muid :: Data.Text.Text
  }

data QidNode = FileNode {fileData :: Data.ByteString.ByteString} | DirNode {dirEntries :: Data.Map.Map Data.Text.Text Msg.QidPath}

data HandlerState = HandlerState
  { fids :: Data.Map.Map Msg.Fid [FidState],
    qidNodes :: Data.Map.Map QidPath QidNode,
    rootDirQidPath :: Msg.QidPath,
    rootDirQidVersion :: [Data.Word.Word8],
    lastQidPath :: Int64,
    maxMsgSize :: Int
  }

fidInUse :: FidState -> Bool
fidInUse (AttachedFid {}) = True
fidInUse (OpenFid {}) = True
fidInUse (ClunkedFid {}) = False

resolve :: HandlerState -> [WalkedQidPath] -> [Data.Text.Text] -> Either (Error, [WalkedQidPath]) [WalkedQidPath]
resolve handlerState currentPath walkNames = go [] currentPath walkNames
  where
    currentQidPath :: [WalkedQidPath] -> QidPath
    currentQidPath (WalkedQidPath {..} : _) = QidPath qidPath
    currentQidPath [] = RootQidPath
    go :: [WalkedQidPath] -> [WalkedQidPath] -> [Data.Text.Text] -> Either (Error, [WalkedQidPath]) [WalkedQidPath]
    go walkResult currentWalkPath (walkName : walkNames) =
      do
        qidNode <- maybe (Left (NoPathEntry, walkResult)) (Right) $ Data.Map.lookup (currentQidPath currentWalkPath) handlerState.qidNodes
        walkQidPath <- case qidNode of
          (DirNode {..}) -> maybe (Left (NoPathEntry, walkResult)) (Right) $ Data.Map.lookup walkName dirEntries
          -- Implement ".."
          (_) -> Left (NoPathEntry, walkResult)
        let newWalkPath = WalkedQidPath {path = walkName, qidPath = walkQidPath}
        go (newWalkPath : walkResult) (newWalkPath : currentWalkPath) walkNames
    go walkResult _currentWalkPath [] = Right walkResult

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
          attachedFid = AttachedFid {walkedQidPath = []}

walkFid :: Data.IORef.IORef HandlerState -> Msg.Fid -> [Data.Text.Text] -> Msg.Fid -> IO (Either (Error, [WalkedQidPath]) [WalkedQidPath])
walkFid fidState fid walkNames newFid = do
  Data.IORef.atomicModifyIORef' fidState $ \currentHandler ->
    case (Data.Map.lookup fid currentHandler.fids) of
      (Just (currentFid@AttachedFid {} : _)) ->
        case (Data.Map.lookup newFid currentHandler.fids) of
          (Just (AttachedFid {} : fidList)) | newFid == fid ->
            case resolve currentHandler currentFid.walkedQidPath walkNames of
              Right paths ->
                (currentHandler {fids = Data.Map.insert newFid (attachedFid : fidList) currentHandler.fids}, Right paths)
                where
                  attachedFid = AttachedFid {walkedQidPath = currentFid.walkedQidPath ++ paths}
              Left (err, partialWalkPath) -> (currentHandler, Left (err, partialWalkPath))
          (Just fidList) | (any fidInUse fidList) -> (currentHandler, Left (FidInUse, []))
          (result) ->
            case resolve currentHandler currentFid.walkedQidPath walkNames of
              Right paths ->
                (currentHandler {fids = Data.Map.insert newFid (attachedFid : fidList) currentHandler.fids}, Right paths)
                where
                  attachedFid = AttachedFid {walkedQidPath = currentFid.walkedQidPath ++ paths}
              Left (err, partialWalkPath) -> (currentHandler, Left (err, partialWalkPath))
            where
              fidList = maybe [] id result
      (Just (OpenFid {} : _)) -> (currentHandler, Left (FidAlreadyOpen, []))
      (Just (ClunkedFid {} : _)) -> (currentHandler, Left (FidNotAttached, []))
      (Just []) -> (currentHandler, Left (FidNotAttached, []))
      (Nothing) -> (currentHandler, Left (FidNotAttached, []))

openFid :: Data.IORef.IORef HandlerState -> Msg.Fid -> Msg.OpenMode -> IO (Either Error FidState)
openFid fidState fid openMode = do
  Data.IORef.atomicModifyIORef' fidState $ \currentHandler ->
    case (Data.Map.lookup fid currentHandler.fids) of
      (Just (currentFid@AttachedFid {} : rest)) ->
        ( currentHandler {fids = Data.Map.insert fid (openedFid : rest) currentHandler.fids},
          Right openedFid
        )
        where
          openedFid = OpenFid {walkedQidPath = currentFid.walkedQidPath, mode = openMode}
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
rootDirStat qid =
  Msg.Stat
    { statType = 0,
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

buildQid :: Data.Word.Word8 -> [Data.Word.Word8] -> Msg.QidPath -> Msg.Qid
buildQid qidType qidVersion qidPath = Msg.Qid $ Data.ByteString.concat [Data.ByteString.pack $ concat [[qidType], qidVersion], qidPath.getQidPath]

newHandler :: Network.Socket.Socket -> IO (Network.Plan9Hask.HandlerParser.Plan9Callback (IO ()))
newHandler sock = do
  let rootDirQidPath :: Msg.QidPath
      rootDirQidPath = Msg.QidPath $ Data.ByteString.pack [7, 0, 0, 0, 0, 0, 0, 0]
      rootDirQidVersion :: [Data.Word.Word8]
      rootDirQidVersion = [1, 0, 0, 0]
      rootQid = buildQid Msg.dirQidType rootDirQidVersion rootDirQidPath
  fidState <-
    Data.IORef.newIORef $
      HandlerState
        { fids = Data.Map.empty,
          qidNodes = Data.Map.empty,
          rootDirQidPath = rootDirQidPath,
          rootDirQidVersion = rootDirQidVersion,
          lastQidPath = 1,
          maxMsgSize = 512
        }
  let defaultIoUnitByteCount = Msg.ByteCount $ 4096
      sendResponse bStrBuilder = Network.Socket.ByteString.Lazy.sendAll sock (Data.ByteString.Builder.toLazyByteString bStrBuilder)
      handleTVersion tag mSize version = do
        let msgSize = (min mSize 8192)
        Data.IORef.atomicModifyIORef' fidState (\currentHandler -> (currentHandler {maxMsgSize = fromIntegral msgSize}, ())) -- Possible overflow
        sendResponse $ Network.Plan9Hask.Builder.rVersionMsgBuilder tag msgSize
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
          (Left (err, [])) -> sendResponse $ Network.Plan9Hask.Builder.rErrorMsgBuilder tag $ Data.Text.show err
          (Left (err, partialWalkPath)) -> sendResponse $ Network.Plan9Hask.Builder.rWalkMsgBuilder tag (map (\p -> buildQid 0 rootDirQidVersion p.qidPath) partialWalkPath)
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
          (Right openFid@OpenFid {}) | openFid.walkedQidPath == [] -> sendResponse $ Network.Plan9Hask.Builder.rReadMsgBuilder tag Data.ByteString.empty -- root dir read
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
          Just (attachedFid@AttachedFid {} : _) | attachedFid.walkedQidPath == [] -> sendResponse $ Network.Plan9Hask.Builder.rStatMsgBuilder tag (rootDirStat rootQid)
          Just (attachedFid@AttachedFid {} : _) -> sendResponse $ Network.Plan9Hask.Builder.rErrorMsgBuilder tag $ Data.Text.show NoPathEntry
          _ -> sendResponse $ Network.Plan9Hask.Builder.rErrorMsgBuilder tag $ Data.Text.show FidNotAttached
      handleTWstat tag fid statByteCount stat = Data.Text.IO.putStrLn $ Data.Text.concat ["tWStat"]
      handleParseError err = Data.Text.IO.putStrLn $ Data.Text.concat ["Parse error ", Data.Text.pack $ show err]
      callback = Network.Plan9Hask.HandlerParser.Plan9Callback {..}
  return callback
