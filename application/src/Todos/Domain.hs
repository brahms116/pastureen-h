{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Todos.Domain
  ( HasTodoistToken (..),
    CreateTodo (..),
    DeleteTodo (..),
    GetTodos (..),
  )
where

import Control.Monad.Trans
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time
import Network.HTTP.Req
import Todos.Types
import Util

type TodoistToken = T.Text

class (Monad m) => HasTodoistToken m where
  getTodoistToken :: m TodoistToken

withTokenOpts :: (HasTodoistToken m) => m (Option 'Https)
withTokenOpts = do
  oAuth2Bearer . TE.encodeUtf8 <$> getTodoistToken

class (Monad m) => DeleteTodo m where
  deleteTodo :: TodoId -> m ()
  default deleteTodo :: (MonadRunHttp m, HasTodoistToken m) => TodoId -> m ()
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
  createTodo :: CreateTodoTask -> m TodoTask
  default createTodo :: (MonadRunHttp m, HasTodoistToken m) => CreateTodoTask -> m TodoTask
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
  -- The todoist api is borked with timezones
  -- So we are not using the isOverDue option
  -- let projectIdOpt = maybe mempty ("project_id" =:) projectId
  --     isOverdueOpt =
  --       maybe
  --         mempty
  --         (\x -> "filter" =: (if x then "overdue" :: T.Text else "!overdue"))
  --         isOverdue
  --  in projectIdOpt <> isOverdueOpt
  maybe mempty ("project_id" =:) projectId

isTodoOverDue :: TodoTask -> UTCTime -> Maybe Bool
isTodoOverDue task now = (`isTodoTimeOverdue` now) <$> tdtDateTime task

applyOverdueFilter :: (MonadIO m) => [TodoTask] -> Maybe Bool -> m [TodoTask]
applyOverdueFilter tasks isOverdue =
  case isOverdue of
    Nothing -> return tasks
    Just overdue ->
      let condition = if overdue then id else not
       in do
            currentTime <- liftIO getCurrentTime
            return $
              filter
                (\x -> maybe False condition (isTodoOverDue x currentTime))
                tasks

class (Monad m) => GetTodos m where
  getTodos :: GetTodoOpts -> m [TodoTask]
  default getTodos :: (MonadRunHttp m, HasTodoistToken m) => GetTodoOpts -> m [TodoTask]
  getTodos opts = do
    option <- (gtoToOpts opts <>) <$> withTokenOpts
    response <-
      runHttpReq $
        req
          GET
          (https "api.todoist.com" /: "rest" /: "v2" /: "tasks")
          NoReqBody
          jsonResponse
          option
    applyOverdueFilter (responseBody response) (gtoIsOverdue opts)
