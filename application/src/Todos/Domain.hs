{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Todos.Domain
  ( HasTodoistToken,
    CreateTodo (..),
    DeleteTodo (..),
    GetTodos (..),
  )
where

import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.HTTP.Req
import Todos.Types
import Util

type TodoistToken = T.Text

class (Monad m) => HasTodoistToken m where
  getTodoistToken :: m TodoistToken

withTokenOpts :: (HasTodoistToken m) => m (Option 'Https)
withTokenOpts = do
  oAuth2Token . TE.encodeUtf8 <$> getTodoistToken

class (Monad m) => DeleteTodo m where
  deleteTodo :: TodoistTaskId -> m ()
  default deleteTodo :: (MonadRunHttp m, HasTodoistToken m) => TodoistTaskId -> m ()
  deleteTodo taskId = do
    option <- withTokenOpts
    _ <-
      runHttpReq $
        req
          DELETE
          (https "api.todoist.com" /: "rest" /: "v2" /: "tasks" /: taskId)
          NoReqBody
          ignoreResponse
          option
    return ()

class CreateTodo m where
  createTodo :: CreateTodoistTask -> m TodoistTask
  default createTodo :: (MonadRunHttp m, HasTodoistToken m) => CreateTodoistTask -> m TodoistTask
  createTodo cdtd = do
    option <- withTokenOpts
    response <-
      runHttpReq $
        req
          POST
          (https "api.todoist.com" /: "rest" /: "v2" /: "tasks")
          (ReqBodyJson cdtd)
          jsonResponse
          option
    return $ responseBody response

gtoToOpts :: GetTodoOpts -> Option 'Https
gtoToOpts (GetTodoOpts projectId isOverdue) =
  let projectIdOpt = maybe mempty ("project_id" =:) projectId
      isOverdueOpt =
        maybe
          mempty
          (\x -> "filter" =: (if x then "overdue" :: T.Text else "!overdue"))
          isOverdue
   in projectIdOpt <> isOverdueOpt

class (Monad m) => GetTodos m where
  getTodos :: GetTodoOpts -> m [TodoistTask]
  default getTodos :: (MonadRunHttp m, HasTodoistToken m) => GetTodoOpts -> m [TodoistTask]
  getTodos opts = do
    option <- (gtoToOpts opts <>) <$> withTokenOpts
    response <- runHttpReq $ req GET (https "api.todoist.com" /: "rest" /: "v2" /: "tasks") NoReqBody jsonResponse option
    return $ responseBody response
