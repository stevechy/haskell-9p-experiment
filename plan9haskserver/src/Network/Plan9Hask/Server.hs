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
import Data.Int
import Data.List.NonEmpty qualified
import Data.Text qualified
import Data.Text.IO qualified
import Network.Plan9Hask.HandlerParser qualified
import Network.Plan9Hask.Parser qualified
import Network.Socket qualified
import Network.Socket.ByteString qualified

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
