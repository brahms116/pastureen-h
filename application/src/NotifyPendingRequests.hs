{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE OverloadedStrings #-}

module NotifyPendingRequests
  ( MonadNotifyPendingRequests (..),
  )
where

import Data.Foldable
import qualified Data.Text as T
import Elvanto
import Notifications

pendingRequestsToNotifications :: ServingRequests -> Maybe Notification
pendingRequestsToNotifications (ServingRequests req sw) =
  let body = (fmtRosterRequest <$> req) <> (fmtSwapRequest <$> sw)
   in if null body
        then Nothing
        else Just (Notification "Pending elvanto requests: " $ T.unlines body)

fmtRosterRequest :: RosterRequest -> T.Text
fmtRosterRequest (RosterRequest role date) =
  T.concat ["Request: ", role, " on ", date]

fmtSwapRequest :: SwapRequest -> T.Text
fmtSwapRequest (SwapRequest role date) =
  T.concat ["Swap request", role, " on ", date]

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
