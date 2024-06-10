{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE OverloadedStrings #-}

module Elvanto
  ( ServingRequests (..),
    ElvantoCreds (..),
    MonadElvantoLogin (..),
    MonadPendingRequests (..),
    HasElvantoCreds (..),
    ServingRole,
    ServingDate,
    RosterRequest (..),
    SwapRequest (..),
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

data RosterRequest = RosterRequest
  { rqRole :: !ServingRole,
    rqDate :: !ServingDate
  }
  deriving (Show, Eq)

instance FromJSON RosterRequest where
  parseJSON = withObject "RosterRequest" $
    \v -> RosterRequest <$> v .: "positionName" <*> v .: "scheduleDateTime"

data SwapRequest = SwapRequest
  { swrRole :: !ServingRole,
    swrDate :: !ServingDate
  }
  deriving (Show, Eq)

instance FromJSON SwapRequest where
  parseJSON = withObject "SwapRequest" $
    \v -> SwapRequest <$> v .: "position" <*> v .: "dateTimeFormated"

data ServingRequests = ServingRequests
  { srRequests :: ![RosterRequest],
    srSwaps :: ![SwapRequest]
  }
  deriving (Show, Eq)

instance FromJSON ServingRequests where
  parseJSON = withObject "ServingRequests" $
    \v -> ServingRequests <$> v .: "scheduleRequests" <*> v .: "swapReplaceRequests"

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

re ::
  BS.ByteString ->
  BS.ByteString ->
  (BS.ByteString, BS.ByteString, BS.ByteString, [BS.ByteString])
re txt pattern = txt =~ pattern

responseServingRoles :: BS.ByteString -> ServingRequests
responseServingRoles body =
  let regex = "Roster.initRequest\\((.*)\\);"
      regexed = case re body regex of
        (_, _, _, x:_ ) -> x
        _error -> error "regex failed"
   in case eitherDecodeStrict regexed of
        Left e -> error e
        Right x -> x

class (Monad m) => MonadPendingRequests m where
  getPendingRequests :: ElvantoSessionCookie -> m ServingRequests
  default getPendingRequests :: (MonadRunHttp m) => ElvantoSessionCookie -> m ServingRequests
  getPendingRequests cookie = do
    let url = https "annst.elvanto.com.au" /: "roster" /: "requests"
    roles <-
      responseServingRoles . responseBody
        <$> runHttpReq (req GET url NoReqBody bsResponse (cookieJar cookie))
    liftIO $ print roles
    return roles
