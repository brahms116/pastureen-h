{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE OverloadedStrings #-}

module OverdueTodos (MonadOverdueTodos (..)) where

import qualified Data.Text as T
import Notifications
import Todos.Domain
import Todos.Types
import Control.Monad
import Control.Applicative

overdueTodosToNotification :: [TodoTask] -> Notification
overdueTodosToNotification todos =
  let title = "Overdue todos"
      body = T.unwords $ (\x -> tdtTitle x <> "!") <$> todos
   in Notification title body

class (Monad m) => MonadOverdueTodos m where
  notifyOverdueTodos :: m ()
  default notifyOverdueTodos :: (Alternative m, MonadSendNotification m, MonadGetTodos m) => m ()
  notifyOverdueTodos = do
    todos <- getTodos $ defaultGetTodoParams {gtpIsOverdue = Just True}
    guard $ not $ null todos
    sendNotification $ overdueTodosToNotification todos
