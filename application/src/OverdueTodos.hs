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
  default notifyOverdueTodos :: (MonadSendNotification m, MonadGetTodos m) => m ()
  notifyOverdueTodos = do
    todos <- getTodos $ defaultGetTodoParams {gtpIsOverdue = Just True}
    case todos of
      [] -> return ()
      _ -> sendNotification $ overdueTodosToNotification todos
