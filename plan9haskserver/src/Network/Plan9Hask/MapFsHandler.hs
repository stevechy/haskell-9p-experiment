{-# LANGUAGE BangPatterns #-}
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
import Data.ByteString.Lazy qualified
import Data.IORef qualified
import Data.Int
import Data.Map qualified
import Data.Text qualified
import Data.Text.Encoding qualified
import Data.Text.IO qualified
import Data.Word qualified
import Network.Plan9Hask.Builder qualified
import Network.Plan9Hask.Error
import Network.Plan9Hask.Error qualified as Error
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
        mode :: Msg.OpenMode,
        cachedQidData :: Maybe Data.ByteString.Lazy.ByteString
      }
  | ClunkedFid {}

data WalkedQidPath = WalkedQidPath
  { path :: Data.Text.Text,
    qidPath :: Msg.QidPath
  }
  deriving (Show, Eq, Ord)

data NodeStat = NodeStat
  { mode :: Msg.Permissions,
    atime :: Msg.EpochSeconds,
    mtime :: Msg.EpochSeconds,
    uid :: Data.Text.Text,
    gid :: Data.Text.Text,
    muid :: Data.Text.Text,
    qidVersion :: [Data.Word.Word8]
  }

data QidNode
  = FileNode
      { fileData :: Data.ByteString.ByteString,
        nodeStat :: NodeStat,
        qidPath :: Msg.QidPath
      }
  | DirNode
      { dirEntries :: Data.Map.Map Data.Text.Text Msg.QidPath,
        nodeStat :: NodeStat,
        qidPath :: Msg.QidPath
      }

qidNodeStat :: QidNode -> NodeStat
qidNodeStat (FileNode {..}) = nodeStat
qidNodeStat (DirNode {..}) = nodeStat

qidNodeData :: HandlerState -> QidNode -> Data.ByteString.Lazy.ByteString
qidNodeData _ (FileNode {..}) = Data.ByteString.Lazy.fromStrict fileData
qidNodeData currentHandler (dirNode@DirNode {}) = Data.ByteString.Builder.toLazyByteString $ mconcat builders
  where
    nodeDataBuilder fileName qidPath qidNode =
      Data.ByteString.Builder.byteString $
        Network.Plan9Hask.Builder.statByteString $
          qidNodeStatMsg currentHandler (WalkedQidPath {path = fileName, qidPath = qidPath}) qidNode
    prependDirStat fileName qidPath statBuilderAcc = case Data.Map.lookup qidPath currentHandler.qidNodes of
      (Just qidNode) -> (nodeDataBuilder fileName qidPath qidNode) : statBuilderAcc
      Nothing -> statBuilderAcc
    builders = Data.Map.foldrWithKey prependDirStat [] dirNode.dirEntries

data HandlerState = HandlerState
  { fids :: Data.Map.Map Msg.Fid [FidState],
    -- Using this representation for qid nodes allows
    -- for bad representations including circular directory loops and orphan nodes.
    -- It also allows hard links to be represented.
    -- Allowing orphan nodes allows the node to persist after the file is removed while there are fids that have the node open.
    -- See https://ericvh.github.io/9p-rfc/rfc9p2000.html#anchor31
    qidNodes :: Data.Map.Map Msg.QidPath QidNode,
    rootDirQidPath :: Msg.QidPath,
    nextQidPath :: Int64,
    maxMsgSize :: Int
  }

fidInUse :: FidState -> Bool
fidInUse (AttachedFid {}) = True
fidInUse (OpenFid {}) = True
fidInUse (ClunkedFid {}) = False

validWalkNewFid :: FidState -> Bool
validWalkNewFid (AttachedFid {}) = False
validWalkNewFid (OpenFid {}) = True
validWalkNewFid (ClunkedFid {}) = False

resolve :: HandlerState -> [WalkedQidPath] -> [Data.Text.Text] -> Either (Error, [WalkedQidPath]) [WalkedQidPath]
resolve handlerState currentPath walkNames = fmap reverse $ go [] (walkedQidPathEndQid handlerState currentPath).qidPath walkNames
  where
    go :: [WalkedQidPath] -> Msg.QidPath -> [Data.Text.Text] -> Either (Error, [WalkedQidPath]) [WalkedQidPath]
    go walkResult currentQidPath (walkName : walkNames) =
      do
        qidNode <- maybe (Left (NoPathEntry, walkResult)) (Right) $ Data.Map.lookup currentQidPath handlerState.qidNodes
        walkQidPath <- case qidNode of
          (DirNode {..}) -> maybe (Left (NoPathEntry, walkResult)) (Right) $ Data.Map.lookup walkName dirEntries
          -- Implement ".."
          (_) -> Left (NoPathEntry, walkResult)
        let newWalkPath = WalkedQidPath {path = walkName, qidPath = walkQidPath}
        go (newWalkPath : walkResult) walkQidPath walkNames
    go walkResult _currentQidPath [] = Right walkResult

attachFid :: Data.IORef.IORef HandlerState -> Msg.Fid -> IO (Either Error FidState)
attachFid fidState fid = do
  Data.IORef.atomicModifyIORef' fidState $ \currentHandler ->
    case (Data.Map.lookup fid currentHandler.fids) of
      (Just fidList) | (any fidInUse fidList) -> (currentHandler, Left $ FidInUseWithFid fid)
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
          -- 9pfuse seems to reuse fids that have been walked but not opened as newFid
          (Just fidList) | (any validWalkNewFid fidList) -> (currentHandler, Left (FidInUseWithFid newFid, []))
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
          qidPath = (walkedQidPathEndQid currentHandler currentFid.walkedQidPath).qidPath
          qidNode = Data.Map.lookup qidPath currentHandler.qidNodes
          openedFid =
            OpenFid
              { walkedQidPath = currentFid.walkedQidPath,
                mode = openMode,
                cachedQidData = fmap (qidNodeData currentHandler) qidNode
              }
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

emptyRootDirNode :: Msg.QidPath -> QidNode
emptyRootDirNode qidPath =
  DirNode
    { dirEntries = Data.Map.empty,
      nodeStat =
        NodeStat
          { mode = Msg.directoryPermission Msg.worldReadblePermission,
            atime = Msg.EpochSeconds $ 0,
            mtime = Msg.EpochSeconds $ 0,
            uid = "root",
            gid = "root",
            muid = "root",
            qidVersion = [1, 0, 0, 0]
          },
      qidPath = qidPath
    }

qidNodeQidType :: QidNode -> Data.Word.Word8
qidNodeQidType (FileNode {}) = Msg.fileQidType
qidNodeQidType (DirNode {}) = Msg.dirQidType

qidNodeStatMsg :: HandlerState -> WalkedQidPath -> QidNode -> Msg.Stat
qidNodeStatMsg currentHandler walkedQidPath qidNode =
  let currentNodeStat = qidNodeStat qidNode
      qid = qidNodeQid qidNode
      readByteCount :: Int
      readByteCount = case qidNode of
        (FileNode {..}) -> Data.ByteString.length fileData
        (DirNode {}) -> 0 -- Directories have a conventional length of 0 https://ericvh.github.io/9p-rfc/rfc9p2000.html#anchor32
   in Msg.Stat
        { statType = 0,
          statDev = 0,
          qid = qid,
          mode = currentNodeStat.mode,
          length = Msg.Offset $ fromIntegral readByteCount,
          fileName = walkedQidPath.path,
          atime = currentNodeStat.atime,
          mtime = currentNodeStat.mtime,
          uid = currentNodeStat.uid,
          gid = currentNodeStat.gid,
          muid = currentNodeStat.muid
        }

qidNodeQid :: QidNode -> Msg.Qid
qidNodeQid qidNode = buildQid (qidNodeQidType qidNode) qidNode.nodeStat.qidVersion qidNode.qidPath

buildQid :: Data.Word.Word8 -> [Data.Word.Word8] -> Msg.QidPath -> Msg.Qid
buildQid qidType qidVersion qidPath = Msg.Qid $ Data.ByteString.concat [Data.ByteString.pack $ concat [[qidType], qidVersion], qidPath.getQidPath]

walkedQidPathQids :: HandlerState -> [WalkedQidPath] -> [Msg.Qid]
walkedQidPathQids handlerState walkPath = foldr accumulateQid [] walkPath
  where
    qidNodes = handlerState.qidNodes
    accumulateQid :: WalkedQidPath -> [Msg.Qid] -> [Msg.Qid]
    accumulateQid walkedQidPath tailQids = case resolveQid walkedQidPath of
      (Just qid) -> qid : tailQids
      Nothing -> []
    resolveQid :: WalkedQidPath -> Maybe Msg.Qid
    resolveQid walkedQidPath =
      case Data.Map.lookup walkedQidPath.qidPath qidNodes of
        (Just qidNode) -> Just $ qidNodeQid qidNode
        (Nothing) -> Nothing

walkedQidPathEndQid :: HandlerState -> [WalkedQidPath] -> WalkedQidPath
walkedQidPathEndQid handlerState [] = WalkedQidPath {path = "/", qidPath = handlerState.rootDirQidPath}
walkedQidPathEndQid _ walkedQids = (last walkedQids)

int64ToQidPath :: Int64 -> Msg.QidPath
int64ToQidPath qidInt64 = Msg.QidPath $ Data.ByteString.Lazy.toStrict $ qidLazyBytes
  where
    qidLazyBytes = Data.ByteString.Builder.toLazyByteString $ Data.ByteString.Builder.int64LE qidInt64

allocateQidPath :: Data.IORef.IORef (HandlerState) -> IO (Msg.QidPath)
allocateQidPath handlerIORef = do
  qidPath <-
    Data.IORef.atomicModifyIORef'
      handlerIORef
      ( \currentHandler ->
          let newQidPathInt64 = currentHandler.nextQidPath
              newQidPathBytes = int64ToQidPath newQidPathInt64
              !updatedNextQidPath = newQidPathInt64 + 1
           in ( currentHandler
                  { nextQidPath = updatedNextQidPath
                  },
                newQidPathBytes
              )
      )
  return qidPath

insertQidNode :: QidNode -> HandlerState -> HandlerState
insertQidNode qidNode currentHandler =
  let !updatedQidNodes = Data.Map.insert qidNode.qidPath qidNode currentHandler.qidNodes
   in currentHandler
        { qidNodes = updatedQidNodes
        }

addQidNodeToDir :: Msg.QidPath -> Data.Text.Text -> QidNode -> HandlerState -> Either Error HandlerState
addQidNodeToDir dirQid fileName childQidNode currentHandler =
  case Data.Map.lookup dirQid currentHandler.qidNodes of
    (Nothing) -> Left NoPathEntry
    (Just dirNode@DirNode {}) ->
      let withChildNode = (insertQidNode childQidNode currentHandler)
          !updatedDirEntries = Data.Map.insert fileName childQidNode.qidPath dirNode.dirEntries
          !updatedDirNode = dirNode {dirEntries = updatedDirEntries}
          updatedHandler = insertQidNode (updatedDirNode) withChildNode
       in Right updatedHandler
    (Just FileNode {}) -> Left NotADir

basicFileQidNode :: Data.ByteString.ByteString -> Msg.QidPath -> QidNode
basicFileQidNode fileData qidPath =
  FileNode
    { nodeStat =
        NodeStat
          { mode = Msg.worldReadblePermission,
            atime = Msg.EpochSeconds $ 0,
            mtime = Msg.EpochSeconds $ 0,
            uid = "root",
            gid = "root",
            muid = "root",
            qidVersion = [1, 0, 0, 0]
          },
      ..
    }

basicDirQidNode :: Msg.QidPath -> QidNode
basicDirQidNode qidPath =
    DirNode {
        dirEntries = Data.Map.empty,
        nodeStat =
            NodeStat
              { mode = Msg.directoryPermission Msg.worldReadblePermission,
                atime = Msg.EpochSeconds $ 0,
                mtime = Msg.EpochSeconds $ 0,
                uid = "root",
                gid = "root",
                muid = "root",
                qidVersion = [1, 0, 0, 0]
              },
        ..
    }

-- Data.IORef.IORef (HandlerState) cannot be reused by multiple connections
-- Each connection needs to have its own id-space for fids
-- fid{1} for connectionA and fid{1} for connectionB are not the same
-- Reuse can happen once per-connection id spaces are implemented
newHandlerState :: IO (Data.IORef.IORef (HandlerState))
newHandlerState = do
  fidState <-
    Data.IORef.newIORef $
      HandlerState
        { fids = Data.Map.empty,
          qidNodes = Data.Map.empty,
          rootDirQidPath = int64ToQidPath 0,
          nextQidPath = 1,
          maxMsgSize = 512
        }
  newRootQidPath <- allocateQidPath fidState
  Data.IORef.atomicModifyIORef'
    fidState
    ( \currentHandler ->
        let updatedHandler = insertQidNode (emptyRootDirNode newRootQidPath) currentHandler
            updatedRootHandler = updatedHandler {rootDirQidPath = newRootQidPath}
         in (updatedRootHandler, ())
    )
  return fidState

addFile :: Data.IORef.IORef (HandlerState) -> [Data.Text.Text] -> Data.Text.Text -> (Msg.QidPath -> QidNode) -> IO (Maybe (Error))
addFile handlerStateRef walkNames fileName nodeBuilder = do
  currentHandler <- Data.IORef.readIORef handlerStateRef
  let resolvedWalk = resolve currentHandler [] walkNames
  case resolvedWalk of
    Left (err, _) -> pure $ Just err
    Right walkQids -> do
      allocateAndAddQid handlerStateRef (walkedQidPathEndQid currentHandler walkQids).qidPath fileName nodeBuilder

allocateAndAddQid :: Data.IORef.IORef (HandlerState) -> Msg.QidPath -> Data.Text.Text -> (Msg.QidPath -> QidNode) -> IO (Maybe (Error))
allocateAndAddQid fidStateRef dirQidPath fileName nodeBuilder = do
  newHelloWorldQidPath <- allocateQidPath fidStateRef
  _err <-
    Data.IORef.atomicModifyIORef'
      fidStateRef
      ( \currentHandler ->
          let newQidNode = nodeBuilder newHelloWorldQidPath
              resultHandler = addQidNodeToDir dirQidPath fileName newQidNode currentHandler
           in case resultHandler of
                (Left err) -> (currentHandler, Just err)
                (Right updatedHandler) -> (updatedHandler, Nothing)
      )
  return _err

newHandler :: Data.IORef.IORef (HandlerState) -> Network.Socket.Socket -> IO (Network.Plan9Hask.HandlerParser.RequestHandler (IO ()))
newHandler fidState sock = do
  let defaultIoUnitByteCount = Msg.ByteCount $ 4096
      sendResponse bStrBuilder = Network.Socket.ByteString.Lazy.sendAll sock (Data.ByteString.Builder.toLazyByteString bStrBuilder)
      sendError :: (Show a) => Msg.Tag -> a -> IO ()
      sendError tag err = do
        Data.Text.IO.putStrLn $ Data.Text.concat ["Sending error", Data.Text.show tag, Data.Text.show err]
        sendResponse $ Network.Plan9Hask.Builder.rErrorMsgBuilder tag $ Data.Text.show err
      handleTVersion tag mSize version = do
        let !msgSize = (min mSize 8192)
        Data.IORef.atomicModifyIORef' fidState (\currentHandler -> (currentHandler {maxMsgSize = fromIntegral msgSize}, ())) -- Possible overflow
        sendResponse $ Network.Plan9Hask.Builder.rVersionMsgBuilder tag msgSize
      handleTAuth tag authFid username attachName = do
        Data.Text.IO.putStrLn $
          Data.Text.concat
            [ "tAuth ",
              (Data.Text.pack $ show tag)
            ]
        sendError tag NotImplemented
      handleTFlush tag oldTag = do
        Data.Text.IO.putStrLn $
          Data.Text.concat
            [ "tFlush ",
              (Data.Text.pack $ show tag)
            ]
        sendError tag NotImplemented
      handleTAttach tag fid authFid username attachName = do
        attachResult <- attachFid fidState fid
        readFidState <- Data.IORef.readIORef fidState
        case attachResult of
          (Left err) -> sendError tag $ Data.Text.show err
          (Right _) -> case Data.Map.lookup readFidState.rootDirQidPath readFidState.qidNodes of
            (Just qidNode) -> sendResponse $ Network.Plan9Hask.Builder.rAttachMsgBuilder tag $ qidNodeQid qidNode
            (Nothing) -> sendError tag $ Data.Text.show Error.NoPathEntry
      handleTWalk :: Msg.Tag -> Msg.Fid -> Msg.Fid -> Msg.ArrayElementCount -> [Data.Text.Text] -> IO ()
      handleTWalk tag fid newFid _walkNameCount walkNames = do
        walkResult <- walkFid fidState fid walkNames newFid
        readFidState <- Data.IORef.readIORef fidState
        case walkResult of
          (Right walkResult) -> do
            Data.Text.IO.putStrLn $ Data.Text.show walkResult
            sendResponse $ Network.Plan9Hask.Builder.rWalkMsgBuilder tag (walkedQidPathQids readFidState walkResult)
          (Left (err, [])) -> sendError tag $ Data.Text.show err
          (Left (err, partialWalkPath)) -> do
            Data.Text.IO.putStrLn $ Data.Text.concat [Data.Text.show err, Data.Text.show walkResult]
            sendResponse $ Network.Plan9Hask.Builder.rWalkMsgBuilder tag (walkedQidPathQids readFidState partialWalkPath)
      handleTOpen tag fid openMode = do
        openResult <- openFid fidState fid openMode
        readFidState <- Data.IORef.readIORef fidState
        case openResult of
          (Left err) -> sendError tag $ Data.Text.show err
          (Right openFid@OpenFid {}) ->
            case Data.Map.lookup (walkedQidPathEndQid readFidState openFid.walkedQidPath).qidPath readFidState.qidNodes of
              (Just qidNode) -> do
                Data.Text.IO.putStrLn $ Data.Text.concat ["Cached Data:", (Data.Text.show openFid.cachedQidData)]
                sendResponse $ Network.Plan9Hask.Builder.rOpenMsgBuilder tag (qidNodeQid qidNode) defaultIoUnitByteCount
              Nothing -> sendError tag $ "Unexpected fid state "
          (Right _) -> sendError tag $ "Unexpected fid state"
      handleTCreate tag fid fileName permissions mode = do
        Data.Text.IO.putStrLn $ Data.Text.concat ["tCreate"]
        sendError tag $ Data.Text.show NotImplemented
      handleTRead :: Msg.Tag -> Msg.Fid -> Msg.Offset -> Msg.ByteCount -> IO ()
      handleTRead tag fid offset readByteCount = do
        readOkResult <- readFid fidState fid
        case readOkResult of
          (Left err) -> sendError tag err
          (Right openFid@OpenFid {cachedQidData = Just qidData}) -> do
            Data.Text.IO.putStrLn $ Data.Text.concat ["Read", Data.Text.show $ Data.ByteString.Lazy.length qidData, Data.Text.show qidData]
            Data.Text.IO.putStrLn $ Data.Text.concat ["ReadBytes", Data.Text.show readBytes]
            sendResponse $ Network.Plan9Hask.Builder.rReadMsgBuilder tag readBytes -- root dir read
            where
              readBytes = Data.ByteString.Lazy.toStrict $ Data.ByteString.Lazy.take (fromIntegral readByteCount.getByteCount) $ Data.ByteString.Lazy.drop (fromIntegral offset.getOffset) qidData -- possible overflow
          (Right openFid@OpenFid {cachedQidData = Nothing}) -> sendError tag NoPathEntry
          (Right _) -> sendError tag FidUnexpectedReadError -- root dir read
      handleTWrite tag fid offset writeByteCount writeData = do
        Data.Text.IO.putStrLn $ Data.Text.concat ["tWrite"]
        sendError tag NotImplemented
      handleTClunk tag fid = do
        clunkResult <- clunkFid fidState fid
        case clunkResult of
          (Left err) -> sendError tag err
          (Right _) -> sendResponse $ Network.Plan9Hask.Builder.rClunkMsgBuilder tag
      handleTRemove tag fid = do
        Data.Text.IO.putStrLn $ Data.Text.concat ["tRemove"]
        sendError tag NotImplemented
      handleTStat tag fid = do
        readFidState <- Data.IORef.readIORef fidState
        case Data.Map.lookup fid readFidState.fids of
          Just (attachedFid@AttachedFid {} : _) ->
            let walkedQidPath = (walkedQidPathEndQid readFidState attachedFid.walkedQidPath)
             in case Data.Map.lookup walkedQidPath.qidPath readFidState.qidNodes of
                  (Just qidNode) -> do
                    let stat = (qidNodeStatMsg readFidState walkedQidPath qidNode)
                    Data.Text.IO.putStrLn $ Data.Text.show stat
                    sendResponse $ Network.Plan9Hask.Builder.rStatMsgBuilder tag stat
                  Nothing -> sendError tag NoPathEntry
          _ -> sendError tag FidNotAttached
      handleTWstat tag fid statByteCount stat = do
        Data.Text.IO.putStrLn $ Data.Text.concat ["tWStat"]
        sendError tag NotImplemented
      handleParseError err = Data.Text.IO.putStrLn $ Data.Text.concat ["Parse error ", Data.Text.pack $ show err]
      callback = Network.Plan9Hask.HandlerParser.RequestHandler {..}
  return callback
