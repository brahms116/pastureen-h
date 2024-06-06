{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Config (TestConfig (..), defaultTestConfig) where

import Control.Monad.Trans
import qualified Data.Text as T
import Notifications
import System.Environment
import Todos.Domain

data TestConfig = TestConfig
  { tcfgTodoistToken :: T.Text,
    tcfgNtfyTopic :: !NotificationTopic
  }
  deriving (Show, Eq)

defaultTestConfig :: (MonadIO m) => m TestConfig
defaultTestConfig = do
  token <- liftIO $ T.pack <$> getEnv "PT_TODOIST_TOKEN"
  topic <- liftIO $ T.pack <$> getEnv "PT_NTFY_TOPIC"
  return $ TestConfig token topic

instance HasTodoistToken TestConfig where
  getTodoistToken = tcfgTodoistToken

instance HasNotificationTopic TestConfig where
  getNotificationTopic = tcfgNtfyTopic
