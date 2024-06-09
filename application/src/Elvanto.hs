{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE OverloadedStrings #-}

module Elvanto
  ( ServingRequest (..),
    ElvantoCreds (..),
    MonadElvantoLogin (..),
    MonadPendingRequests (..),
    HasElvantoCreds (..),
  )
where

import Control.Monad.Reader
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Time
import Network.HTTP.Req
import qualified Network.HTTP.Client as HTTP
import Util

type ServingRole = T.Text

type ElvantoSessionCookie = HTTP.CookieJar

data ServingRequest = ServingRequest
  { srRole :: !ServingRole,
    srDate :: !Day
  }
  deriving (Show, Eq)

data ElvantoCreds = ElvantoCreds
  { ecEmail :: !T.Text,
    ecPassword :: !T.Text
  }
  deriving (Show, Eq)

class HasElvantoCreds a where
  getElvantoCreds :: a -> ElvantoCreds

class (Monad m) => MonadElvantoLogin m where
  elvantoLogin :: m ElvantoSessionCookie
  default elvantoLogin :: (MonadRunHttp m, MonadReader t m, HasElvantoCreds t) => m ElvantoSessionCookie
  elvantoLogin = do
    ElvantoCreds e p <- asks getElvantoCreds
    let url = https "annst.elvanto.com.au" /: "login"
    let body = ReqBodyUrlEnc $ "login_username" =: e <> "login_password" =: p
    response <- runHttpReq $ req POST url body bsResponse mempty
    liftIO $ print $ responseCookieJar response
    return $ responseCookieJar response

class (Monad m) => MonadPendingRequests m where
  getPendingRequests :: ElvantoSessionCookie -> m [ServingRequest]
