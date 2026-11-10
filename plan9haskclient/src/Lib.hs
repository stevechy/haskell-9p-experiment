module Lib
    ( someFunc
    ) where

import Network.Plan9Hask.Client qualified

someFunc :: IO ()
someFunc = do
    putStrLn "someFunc Client"
    Network.Plan9Hask.Client.runClient
    return ()
