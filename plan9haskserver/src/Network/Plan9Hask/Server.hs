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
import Data.IORef qualified
import Data.Int
import Data.List.NonEmpty qualified
import Data.Text qualified
import Data.Text.IO qualified
import Data.Word qualified
import Network.Plan9Hask.Builder qualified
import Network.Plan9Hask.HandlerParser qualified
import Network.Plan9Hask.MinimalHandler qualified
import Network.Plan9Hask.Msg qualified as Msg
import Network.Plan9Hask.Parser qualified
import Network.Socket qualified
import Network.Socket.ByteString qualified
import Data.List (intercalate, intersperse)

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
  let
      buildText = Data.Text.concat . (intersperse " ")
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

data HandlerState
  = FreshParse
      { buffer :: Data.ByteString.ByteString
      }
  | PartialParse
      { partial :: (Data.ByteString.ByteString -> Data.Attoparsec.ByteString.IResult Data.ByteString.ByteString (Int, Data.ByteString.ByteString))
      }

handler :: Network.Socket.Socket -> IO ()
handler sock = runHandler
  where
    runHandler = do
      msgHandler <- Network.Plan9Hask.MinimalHandler.newHandler sock
      go sock msgHandler (FreshParse {buffer = Data.ByteString.empty})
    go :: Network.Socket.Socket -> Network.Plan9Hask.HandlerParser.Plan9Callback (IO ()) -> HandlerState -> IO ()
    go socket msgHandler handlerState = do
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
          go socket msgHandler (FreshParse {buffer = remainder})
        (Data.Attoparsec.ByteString.Partial cont) -> do
          putStrLn $ "Partial"
          go socket msgHandler (PartialParse {partial = cont})
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
