{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Network.Wai.Handler.Warp
import Web.Scotty

main :: IO ()
main = scottyOpts (Options 1 (setHost "0.0.0.0" (setPort 8080 defaultSettings))) $ do
  get "/" $ do
    text "Hello, world!"
