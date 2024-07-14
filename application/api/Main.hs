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

type API = Get '[HTML] H.Html :<|> "static" :> Raw

withHead :: T.Text -> H.Html -> H.Html
withHead title body =
  [shamlet|
    <html lang="en">
      <head>
        <meta charset="UTF-8">
        <meta name="viewport" content="width=device-width, initial-scale=1.0">
        <link rel="preconnect" href="https://fonts.googleapis.com">
        <link rel="preconnect" href="https://fonts.gstatic.com" crossorigin>
        <link href="https://fonts.googleapis.com/css2?family=PT+Mono&display=swap" rel="stylesheet">

        <link rel="stylesheet" href="https://unpkg.com/tailwindcss@3.3.3/src/css/preflight.css">
        <link rel="stylesheet" href="/static/styles.css">
        <title>#{title}
      <body>
        ^{body}
  |]

server :: Server API
server =
  let homepage = return $ withHead "Home" [shamlet|<h1>Welcome to my website!|]
      static = serveDirectoryWebApp "public"
   in homepage :<|> static

app :: Application
app = serve (Proxy :: Proxy API) server

main :: IO ()
main = run 8080 app
