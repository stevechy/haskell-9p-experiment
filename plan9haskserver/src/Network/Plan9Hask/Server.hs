{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoFieldSelectors #-}

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
import Data.Text.Encoding qualified
import Data.Text.IO qualified
import Network.Plan9Hask.Error qualified
import Network.Plan9Hask.HandlerParser qualified
import Network.Plan9Hask.MapFsHandler qualified
import Network.Plan9Hask.MinimalHandler qualified
import Network.Plan9Hask.Parser qualified
import Network.Plan9Hask.ShowHandler qualified
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

data HandlerState
  = FreshParse
      { buffer :: Data.ByteString.ByteString
      }
  | PartialParse
      { partial :: (Data.ByteString.ByteString -> Data.Attoparsec.ByteString.IResult Data.ByteString.ByteString (Int, Data.ByteString.ByteString))
      }

logAndSwallowError :: IO (Maybe Network.Plan9Hask.Error.Error) -> IO ()
logAndSwallowError action = do
  result <- action
  case result of
    Just err -> Data.Text.IO.putStrLn $ Data.Text.concat ["Error", Data.Text.show err]
    Nothing -> pure ()

handler :: Network.Socket.Socket -> IO ()
handler sock = runHandler
  where
    showHandler = Network.Plan9Hask.ShowHandler.handler
    runHandler = do
      _msgHandler <- Network.Plan9Hask.MinimalHandler.newHandler sock
      fidState <- Network.Plan9Hask.MapFsHandler.newHandlerState
      logAndSwallowError $ Network.Plan9Hask.MapFsHandler.addFile fidState [] "hello.txt" (Network.Plan9Hask.MapFsHandler.basicFileQidNode $ Data.Text.Encoding.encodeUtf8 "Hello World\n")
      logAndSwallowError $ Network.Plan9Hask.MapFsHandler.addFile fidState [] "aTestDir" Network.Plan9Hask.MapFsHandler.basicDirQidNode
      logAndSwallowError $ Network.Plan9Hask.MapFsHandler.addFile fidState ["aTestDir"] "aTestFile.txt" (Network.Plan9Hask.MapFsHandler.basicFileQidNode $ Data.Text.Encoding.encodeUtf8 "My test File\n")
      mapHandler <- Network.Plan9Hask.MapFsHandler.newHandler fidState sock
      go sock mapHandler (FreshParse {buffer = Data.ByteString.empty})
    go :: Network.Socket.Socket -> Network.Plan9Hask.HandlerParser.RequestHandler (IO ()) -> HandlerState -> IO ()
    go socket plan9handler handlerState = do
      putStrLn "Handling"
      parseResult <- case handlerState of
        (FreshParse {..}) -> pure $ Network.Plan9Hask.Parser.runMsgParser $ buffer
        (PartialParse {..}) -> do
          -- Using a small recv size to try to expose leftover handling errors
          msg <- Network.Socket.ByteString.recv socket 512
          -- partial seems to return a fail when msg is empty which is what we want
          pure $ partial msg
      case parseResult of
        (Data.Attoparsec.ByteString.Done remainder (size, msgBody)) -> do
          let parsedInt64 :: Int64 = fromIntegral size
          putStrLn $ "Success" ++ (show parsedInt64)
          Data.Text.IO.putStrLn $ Network.Plan9Hask.HandlerParser.handleMsgBody showHandler msgBody
          Network.Plan9Hask.HandlerParser.handleMsgBody plan9handler msgBody
          -- There could be more Done elements in the remainder.
          -- These should be consumed before the next `recv`
          go socket plan9handler (FreshParse {buffer = remainder})
        (Data.Attoparsec.ByteString.Partial cont) -> do
          putStrLn $ "Partial"
          go socket plan9handler (PartialParse {partial = cont})
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
serverHints =
  Network.Socket.defaultHints
    { Network.Socket.addrFlags = [Network.Socket.AI_PASSIVE],
      Network.Socket.addrSocketType = Network.Socket.Stream
    }

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
