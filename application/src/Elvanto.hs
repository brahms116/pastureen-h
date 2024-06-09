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
-- import Data.Time

import Data.Aeson
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T
import qualified Network.HTTP.Client as HTTP
import Network.HTTP.Req
import Text.Regex.TDFA ((=~))
import Util

type ServingRole = T.Text

type ElvantoSessionCookie = HTTP.CookieJar

-- Can't be stuffed parsing atm
-- type ServingDate = Day
type ServingDate = T.Text

data ServingRequest = ServingRequest
  { srRole :: !ServingRole,
    srDate :: !ServingDate
  }
  deriving (Show, Eq)

instance FromJSON ServingRequest where
  parseJSON = withObject "ServingRequest" $
    \v -> ServingRequest <$> v .: "positionName" <*> v .: "scheduleDateTime"

data ServingRequestsJson = ServingRolesJson
  { srjRequests :: ![ServingRequest]
  }
  deriving (Show, Eq)

instance FromJSON ServingRequestsJson where
  parseJSON = withObject "ServingRequestsJson" $
    \v -> ServingRolesJson <$> v .: "scheduleRequests"

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
    return $ responseCookieJar response

re :: BS.ByteString -> BS.ByteString -> [[BS.ByteString]]
re txt pattern = txt =~ pattern

responseServingRoles :: BS.ByteString -> ServingRequestsJson
responseServingRoles body =
  let regex = "Roster.initRequest\\((.*)\\);"
      regexed = case re body regex of
        [[_, x]] -> x
        _error -> error "regex failed"
   in case eitherDecodeStrict regexed of
        Left e -> error e
        Right x -> x

class (Monad m) => MonadPendingRequests m where
  getPendingRequests :: ElvantoSessionCookie -> m [ServingRequest]
  default getPendingRequests :: (MonadRunHttp m) => ElvantoSessionCookie -> m [ServingRequest]
  getPendingRequests cookie = do
    let url = https "annst.elvanto.com.au" /: "roster" /: "requests"
    roles <-
      responseServingRoles . responseBody
        <$> runHttpReq (req GET url NoReqBody bsResponse (cookieJar cookie))
    liftIO $ print roles
    return $ srjRequests roles
