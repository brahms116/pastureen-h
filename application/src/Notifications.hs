{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE OverloadedStrings #-}

module Notifications
  ( Notification (..),
    NotificationTitle,
    NotificationBody,
    NotificationTopic,
    SendNotificationPayload (..),
    notificationToPayload,
    HasNotificationTopic (..),
    HasNotification (..),
    HasNotificationBody (..),
    MonadSendNotification (..),
  )
where

import Control.Monad.Reader
import Data.Aeson
import qualified Data.Text as T
import Network.HTTP.Req
import Util

type NotificationTitle = T.Text

type NotificationBody = T.Text

type NotificationTopic = T.Text

data Notification = Notification
  { ntTitle :: !NotificationTitle,
    ntBody :: !NotificationBody
  }
  deriving (Show)

data SendNotificationPayload = SendNotificationPayload
  { snpTopic :: !NotificationTopic,
    snpTitle :: !NotificationTitle,
    snpBody :: !NotificationBody
  }
  deriving (Show)

instance ToJSON SendNotificationPayload where
  toJSON (SendNotificationPayload topic title body) =
    object
      [ "topic" .= topic,
        "title" .= title,
        "message" .= body
      ]

notificationToPayload :: NotificationTopic -> Notification -> SendNotificationPayload
notificationToPayload topic (Notification title body) =
  SendNotificationPayload
    { snpTopic = topic,
      snpTitle = title,
      snpBody = body
    }

class HasNotificationTopic a where
  getNotificationTopic :: a -> NotificationTopic

instance HasNotificationTopic SendNotificationPayload where
  getNotificationTopic = snpTopic

class HasNotification a where
  getNotification :: a -> Notification

instance HasNotification SendNotificationPayload where
  getNotification (SendNotificationPayload _ title body) = Notification title body

instance HasNotification Notification where
  getNotification = id

class HasNotificationBody a where
  getNotificationBody :: a -> NotificationBody

instance HasNotificationBody SendNotificationPayload where
  getNotificationBody = snpBody

class (Monad m) => MonadSendNotification m where
  sendNotification :: (HasNotification a) => a -> m ()
  default sendNotification :: (MonadRunHttp m, HasNotification a, MonadReader t m, HasNotificationTopic t) => a -> m ()
  sendNotification nt = do
    topic <- asks getNotificationTopic
    let payload = notificationToPayload topic (getNotification nt)
    _ <-
      runHttpReq $
        req
          POST
          (https "nfty")
          (ReqBodyJson payload)
          ignoreResponse
          mempty
    return ()
