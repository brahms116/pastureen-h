{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeOperators #-}

module Main (main) where

import qualified Data.Text as T
import Network.Wai.Handler.Warp
import Servant
import Servant.HTML.Blaze
import qualified Text.Blaze.Html as H
import Text.Hamlet

type API = Get '[HTML] H.Html

withHead :: T.Text -> H.Html -> H.Html
withHead title body =
  [shamlet|
    <html lang="en">
      <head>
        <meta charset="UTF-8">
        <meta name="viewport" content="width=device-width, initial-scale=1.0">
        <title>#{title}
      <body>
        ^{body}
  |]

server :: Server API
server = return $ withHead "Home" [shamlet|<h1>Welcome to my website!|]

app :: Application
app = serve (Proxy :: Proxy API) server

main :: IO ()
main = run 8080 app
