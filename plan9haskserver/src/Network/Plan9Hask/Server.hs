{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.Plan9Hask.Server
  ( serve9p,
  )
where

import Control.Concurrent.Async qualified
import Control.Exception qualified
import Control.Monad qualified
import Data.Attoparsec.ByteString qualified
import Data.ByteString qualified
import Data.ByteString.Builder qualified
import Data.Int
import Data.List.NonEmpty qualified
import Data.Text qualified
import Data.Text.IO qualified
import Data.Word qualified
import Network.Plan9Hask.HandlerParser qualified
import Network.Plan9Hask.Parser qualified
import Network.Plan9Hask.Builder qualified
import Network.Plan9Hask.Msg qualified as Msg
import Network.Socket qualified
import Network.Socket.ByteString qualified
import Network.Socket.ByteString.Lazy qualified


openServerSocket :: Network.Socket.AddrInfo -> IO (Network.Socket.Socket)
openServerSocket addr = Control.Exception.bracketOnError
  (Network.Socket.openSocket addr)
  Network.Socket.close
  $ \sock -> do
    Network.Socket.setSocketOption sock Network.Socket.ReuseAddr 1
    Network.Socket.bind sock $ Network.Socket.addrAddress addr
    Network.Socket.listen sock 1024

    return sock

showParser :: Network.Plan9Hask.HandlerParser.Plan9Callback Data.Text.Text
showParser =
  let handleTVersion tag mSize version =
        Data.Text.concat
          [ "tVersion ",
            (Data.Text.pack $ show tag),
            " m ",
            (Data.Text.pack $ show mSize),
            " v ",
            (Data.Text.pack $ show version)
          ]
      handleTAuth tag authFid username attachName = Data.Text.concat ["tAuth"]
      handleTFlush tag oldTag = Data.Text.concat ["tFlush"]
      handleTAttach tag fid authFid username attachName = Data.Text.concat ["tAttach"]
      handleTWalk tag fid newFid walkNameCount walkNames = Data.Text.concat ["tWalk"]
      handleTOpen tag fid openMode = Data.Text.concat ["tOpen"]
      handleTCreate tag fid fileName permissions mode = Data.Text.concat ["tCreate"]
      handleTRead tag fid offset readByteCount = Data.Text.concat ["tRead"]
      handleTWrite tag fid offset writeByteCount writeData = Data.Text.concat ["tWrite"]
      handleTClunk tag fid = Data.Text.concat ["tClunk"]
      handleTRemove tag fid = Data.Text.concat ["tRemove"]
      handleTStat tag fid = Data.Text.concat ["tStat"]
      handleTWstat tag fid statByteCount stat = Data.Text.concat ["tWStat"]
      handleParseError err = Data.Text.concat ["Parse error ", Data.Text.pack $ show err]
   in Network.Plan9Hask.HandlerParser.Plan9Callback {..}

minimalHandler :: Network.Socket.Socket -> Network.Plan9Hask.HandlerParser.Plan9Callback (IO ())
minimalHandler sock =
  let
      rootDirQidPath :: [Data.Word.Word8]
      rootDirQidPath = [0,0,0,0,0,0,0,7]
      rootQid = Msg.Qid $ Data.ByteString.pack $ concat [[Msg.dirQidType], [0,0,0,0], rootDirQidPath]
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
            Data.Text.IO.putStrLn $ Data.Text.concat ["tAttach",
                                                        (Data.Text.pack $ show tag),
                                                        " m "]
            sendResponse $ Network.Plan9Hask.Builder.rAttachMsgBuilder tag rootQid
      handleTWalk :: Msg.Tag -> Msg.Fid -> Msg.Fid -> Msg.ArrayElementCount -> [Data.Text.Text] -> IO ()
      handleTWalk tag fid newFid walkNameCount walkNames = do
            Data.Text.IO.putStrLn $ Data.Text.concat ["tWalk"]
            if walkNameCount.getArrayElementCount == 0
                then sendResponse $ Network.Plan9Hask.Builder.rWalkMsgBuilder tag []
                else return ()
      handleTOpen tag fid openMode = Data.Text.IO.putStrLn $ Data.Text.concat ["tOpen"]
      handleTCreate tag fid fileName permissions mode = Data.Text.IO.putStrLn $ Data.Text.concat ["tCreate"]
      handleTRead tag fid offset readByteCount = Data.Text.IO.putStrLn $ Data.Text.concat ["tRead"]
      handleTWrite tag fid offset writeByteCount writeData = Data.Text.IO.putStrLn $ Data.Text.concat ["tWrite"]
      handleTClunk tag fid = Data.Text.IO.putStrLn $ Data.Text.concat ["tClunk"]
      handleTRemove tag fid = Data.Text.IO.putStrLn $ Data.Text.concat ["tRemove"]
      handleTStat tag fid = Data.Text.IO.putStrLn $ Data.Text.concat ["tStat"]
      handleTWstat tag fid statByteCount stat = Data.Text.IO.putStrLn $ Data.Text.concat ["tWStat"]
      handleParseError err = Data.Text.IO.putStrLn $ Data.Text.concat ["Parse error ", Data.Text.pack $ show err]
   in Network.Plan9Hask.HandlerParser.Plan9Callback {..}

data HandlerState
  = FreshParse
      { buffer :: Data.ByteString.ByteString
      }
  | PartialParse
      { partial :: (Data.ByteString.ByteString -> Data.Attoparsec.ByteString.IResult Data.ByteString.ByteString (Int, Data.ByteString.ByteString))
      }

handler :: Network.Socket.Socket -> IO ()
handler sock = go sock (FreshParse {buffer = Data.ByteString.empty})
  where
    msgHandler = minimalHandler sock
    go socket handlerState = do
      putStrLn "Handling"
      msg <- Network.Socket.ByteString.recv socket 512
      let parseResult = case handlerState of
            (FreshParse {..}) -> Network.Plan9Hask.Parser.runMsgParser $ Data.ByteString.append buffer msg
            (PartialParse {..}) -> partial msg
      case parseResult of
        (Data.Attoparsec.ByteString.Done remainder (size, msgBody)) -> do
          let parsedInt64 :: Int64 = fromIntegral size
          putStrLn $ "Success" ++ (show parsedInt64)
          Data.Text.IO.putStrLn $ Network.Plan9Hask.HandlerParser.handleMsgBody showParser msgBody
          Network.Plan9Hask.HandlerParser.handleMsgBody msgHandler msgBody
          go socket (FreshParse {buffer = remainder})
        (Data.Attoparsec.ByteString.Partial cont) -> do
          putStrLn $ "Partial"
          go socket (PartialParse {partial = cont})
        (Data.Attoparsec.ByteString.Fail _ _ _) -> putStrLn "Fail"

spawnHandler :: Network.Socket.Socket -> IO ()
spawnHandler socket = Control.Exception.bracketOnError
  (Network.Socket.accept socket)
  (\(sock, _) -> Network.Socket.close sock)
  $ \(sock, _sockAddr) -> do
    putStrLn "Accepted"
    _asyncHandle <- Control.Concurrent.Async.async (handler sock)
    return ()

serverHints :: Network.Socket.AddrInfo
serverHints = Network.Socket.defaultHints {Network.Socket.addrFlags = [Network.Socket.AI_PASSIVE], Network.Socket.addrSocketType = Network.Socket.Stream}

serverThread :: IO ()
serverThread = do
  putStrLn "Init"
  addr <- (Network.Socket.getAddrInfo (Just serverHints) (Nothing) (Just "8564")) :: IO (Data.List.NonEmpty.NonEmpty Network.Socket.AddrInfo)
  _ <- Control.Exception.bracket (openServerSocket (Data.List.NonEmpty.head addr)) (Network.Socket.close) $ \sock -> do
    Control.Monad.forever $ spawnHandler sock
  return ()

serve9p :: IO ()
serve9p = do
  putStrLn "Serving 9p"
  Control.Concurrent.Async.withAsync serverThread $ \asyncHandle -> do
    putStrLn "Waiting"
    Control.Concurrent.Async.wait asyncHandle
  putStrLn "Done"
  return ()
