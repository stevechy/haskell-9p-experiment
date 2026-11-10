module Network.Plan9Hask.Server
  ( serve9p,
  )
where

import Control.Concurrent.Async qualified
import Network.Socket qualified
import Network.Socket.ByteString qualified
import Data.List.NonEmpty qualified
import Control.Exception qualified
import Control.Monad qualified

openServerSocket :: Network.Socket.AddrInfo -> IO (Network.Socket.Socket)
openServerSocket addr = Control.Exception.bracketOnError
    (Network.Socket.openSocket addr)
    Network.Socket.close
    $ \sock -> do
        Network.Socket.setSocketOption sock Network.Socket.ReuseAddr 1
        Network.Socket.bind sock $ Network.Socket.addrAddress addr
        Network.Socket.listen sock 1024

        return sock

handler socket = do
    putStrLn "Handling"
    msg <- Network.Socket.ByteString.recv socket 512
    return ()

spawnHandler socket = Control.Exception.bracketOnError
    (Network.Socket.accept socket)
    (\ (sock, _) -> Network.Socket.close sock)
    $ \ (sock, sockAddr) -> do
        putStrLn "Accepted"
        Control.Concurrent.Async.withAsync (handler sock) $ \ asyncHandle -> pure ()

serverHints = Network.Socket.defaultHints {Network.Socket.addrFlags = [Network.Socket.AI_PASSIVE], Network.Socket.addrSocketType = Network.Socket.Stream}

serverThread = do
  putStrLn "Init"
  addr <- (Network.Socket.getAddrInfo (Just serverHints) (Nothing) (Just "8564")) :: IO (Data.List.NonEmpty.NonEmpty Network.Socket.AddrInfo)
  _ <- Control.Exception.bracket (openServerSocket ( Data.List.NonEmpty.head addr )) (Network.Socket.close) $ \sock -> do
    Control.Monad.forever $ spawnHandler sock
  return ()

serve9p :: IO ()
serve9p = do
  putStrLn "Serving 9p"
  serveResult <- Control.Concurrent.Async.withAsync serverThread $ \asyncHandle -> do
    putStrLn "Waiting"
    Control.Concurrent.Async.wait asyncHandle
  putStrLn "Done"
  return ()
