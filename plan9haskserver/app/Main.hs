module Main (main) where

import Network.Plan9Hask.Server qualified

main :: IO ()
main = Network.Plan9Hask.Server.serve9p
