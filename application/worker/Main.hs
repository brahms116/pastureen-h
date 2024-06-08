{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad.Reader
import qualified Data.Text as T
import Notifications
import OverdueTodos
import System.Environment
import Todos.Domain
import Control.Applicative
import Util

data PtTaskName = NotifiyOverdueTodos deriving (Show)

getPtTaskName :: IO PtTaskName
getPtTaskName = do
  taskName <- getEnv "PT_TASK_NAME"
  return $ case taskName of
    "NOTIFY_OVERDUE_TODOS" -> NotifiyOverdueTodos
    t -> error $ "Invalid task name: " <> t

data OverdueTodoConfig = OverdueTodoConfig
  { odtCfgTodoistToken :: !TodoistToken,
    odtCfgNtfyTopic :: !NotificationTopic
  } deriving (Show)

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

main :: IO ()
main = do
  taskName <- getPtTaskName
  putStrLn $ "Running task: " ++ show taskName
  case taskName of
    NotifiyOverdueTodos -> runOverdueTodoTask
  putStrLn "Task completed"
