{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Applicative
import Control.Monad.Reader
import qualified Data.Text as T
import Elvanto
import Notifications
import OverdueTodos
import System.Environment
import Todos.Domain
import Util

data PtTaskName = NotifiyOverdueTodos | PendingServingRequests deriving (Show)

getPtTaskName :: IO PtTaskName
getPtTaskName = do
  taskName <- getEnv "PT_TASK_NAME"
  return $ case taskName of
    "NOTIFY_OVERDUE_TODOS" -> NotifiyOverdueTodos
    "PENDING_SERVING_REQUESTS" -> PendingServingRequests
    t -> error $ "Invalid task name: " <> t

data OverdueTodoConfig = OverdueTodoConfig
  { odtCfgTodoistToken :: !TodoistToken,
    odtCfgNtfyTopic :: !NotificationTopic
  }
  deriving (Show)

defaultOdtCfg :: IO OverdueTodoConfig
defaultOdtCfg = do
  token <- T.pack <$> getEnv "PT_TODOIST_TOKEN"
  topic <- T.pack <$> getEnv "PT_NTFY_TOPIC"
  return $ OverdueTodoConfig token topic

instance HasTodoistToken OverdueTodoConfig where
  getTodoistToken = odtCfgTodoistToken

instance HasNotificationTopic OverdueTodoConfig where
  getNotificationTopic = odtCfgNtfyTopic

newtype OverdueTodoAppM a = OverdueTodoAppM
  { unOverdueTodoAppM :: ReaderT OverdueTodoConfig IO a
  }
  deriving newtype
    ( Functor,
      Applicative,
      Alternative,
      Monad,
      MonadIO,
      MonadReader OverdueTodoConfig
    )
  deriving anyclass
    ( MonadRunHttp,
      MonadSendNotification,
      MonadGetTodos,
      MonadOverdueTodos
    )

runOverdueTodoTask :: IO ()
runOverdueTodoTask = do
  cfg <- defaultOdtCfg
  runReaderT (unOverdueTodoAppM notifyOverdueTodos) cfg

data PendingServingRequestsConfig = PendingServingRequestsConfig
  { psrCfgElvantoCreds :: !ElvantoCreds,
    psrCfgNtfyTopic :: !NotificationTopic
  }
  deriving (Show)

instance HasElvantoCreds PendingServingRequestsConfig where
  getElvantoCreds = psrCfgElvantoCreds

instance HasNotificationTopic PendingServingRequestsConfig where
  getNotificationTopic = psrCfgNtfyTopic

defaultPsrCfg :: IO PendingServingRequestsConfig
defaultPsrCfg = do
  email <- T.pack <$> getEnv "PT_ELVANTO_EMAIL"
  password <- T.pack <$> getEnv "PT_ELVANTO_PASSWORD"
  topic <- T.pack <$> getEnv "PT_NTFY_TOPIC"
  return $ PendingServingRequestsConfig (ElvantoCreds email password) topic

newtype PendingServingRequestsAppM a = PendingServingRequestsAppM
  { unPendingServingRequestsAppM :: ReaderT PendingServingRequestsConfig IO a
  }
  deriving newtype
    ( Functor,
      Applicative,
      Alternative,
      Monad,
      MonadIO,
      MonadReader PendingServingRequestsConfig
    )
  deriving anyclass
    ( MonadRunHttp,
      MonadSendNotification,
      MonadElvantoLogin,
      MonadPendingRequests
    )

runPendingServingRequestsTask :: IO ()
runPendingServingRequestsTask = do
  cfg <- defaultPsrCfg
  runReaderT
    ( unPendingServingRequestsAppM $ do
        cookie <- elvantoLogin
        requests <- getPendingRequests cookie
        return ()
    )
    cfg

main :: IO ()
main = do
  taskName <- getPtTaskName
  putStrLn $ "Running task: " ++ show taskName
  case taskName of
    NotifiyOverdueTodos -> runOverdueTodoTask
    PendingServingRequests -> runPendingServingRequestsTask
  putStrLn "Task completed"
