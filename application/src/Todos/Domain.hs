{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Todos.Domain (HasTodoistToken, CreateTodo (..), DeleteTodo (..)) where

import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.HTTP.Req
import Todos.Types
import Util

type TodoistToken = String

class (Monad m) => HasTodoistToken m where
  getTodoistToken :: m TodoistToken

withTokenOpts :: (HasTodoistToken m) => m (Option scheme)
withTokenOpts = do
  token <- getTodoistToken
  return $ header "Authorization" $ TE.encodeUtf8 $ T.pack $ "Bearer " ++ token

class (Monad m) => DeleteTodo m where
  deleteTodo :: TodoistTaskId -> m ()
  default deleteTodo :: (MonadRunHttp m, HasTodoistToken m) => TodoistTaskId -> m ()
  deleteTodo taskId = do
    option <- withTokenOpts
    _ <-
      runHttpReq $
        req
          DELETE
          (https "api.todoist.com" /: "rest" /: "v2" /: "tasks" /: T.pack taskId)
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

class GetTodos m where
  getTodos :: m [TodoistTask]
  default getTodos :: (MonadRunHttp m, HasTodoistToken m) => m [TodoistTask]
  getTodos = do
    option <- withTokenOpts
    undefined
