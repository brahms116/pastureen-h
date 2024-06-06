{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Config (TestConfig (..), defaultTestConfig) where

import qualified Data.Text as T
import System.Environment
import Control.Monad.Trans
import Todos.Domain

data TestConfig = TestConfig
  { tcfgtpdoistToken :: T.Text
  }
  deriving (Show, Eq)

defaultTestConfig :: (MonadIO m) => m TestConfig
defaultTestConfig = do
  token <- liftIO $ T.pack <$> getEnv "PT_TODOIST_TOKEN"
  return $ TestConfig token

instance HasTodoistToken TestConfig where
  getTodoistToken = tcfgtpdoistToken
