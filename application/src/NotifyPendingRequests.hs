{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE OverloadedStrings #-}

module NotifyPendingRequests
  (
    MonadNotifyPendingRequests (..),
  )
where

import Data.Foldable
import qualified Data.Text as T
import Elvanto
import Notifications

pendingRequestsToNotifications :: [ServingRequest] -> Maybe Notification
pendingRequestsToNotifications [] = Nothing
pendingRequestsToNotifications requests =
  let title = "Pending church serving requests in elvanto!"
      body = T.unwords $ (\x -> srRole x <> "!") <$> requests
   in Just $ Notification title body

class (Monad m) => MonadNotifyPendingRequests m where
  notifyPendingRequests :: m ()
  default notifyPendingRequests ::
    ( MonadElvantoLogin m,
      MonadPendingRequests m,
      MonadSendNotification m
    ) =>
    m ()
  notifyPendingRequests = do
    cookie <- elvantoLogin
    requests <- getPendingRequests cookie
    forM_
      (pendingRequestsToNotifications requests)
      sendNotification
