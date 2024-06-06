{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Todos.Domain
  ( HasTodoistToken (..),
    MonadCreateTodo (..),
    MonadDeleteTodo (..),
    MonadGetTodos (..),
  )
where

import Control.Monad.Reader
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time
import Network.HTTP.Req
import Todos.Types
import Util

type TodoistToken = T.Text

class HasTodoistToken m where
  getTodoistToken :: m -> TodoistToken

withTokenOpts :: (MonadReader t m, HasTodoistToken t) => m (Option 'Https)
withTokenOpts = do
  asks ((oAuth2Bearer . TE.encodeUtf8) . getTodoistToken)

class (Monad m) => MonadDeleteTodo m where
  deleteTodo :: TodoId -> m ()
  default deleteTodo :: (MonadRunHttp m, MonadReader t m, HasTodoistToken t) => TodoId -> m ()
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

class (Monad m) => MonadCreateTodo m where
  createTodo :: CreateTodoTask -> m TodoTask
  default createTodo :: (MonadRunHttp m, MonadReader t m, HasTodoistToken t) => CreateTodoTask -> m TodoTask
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
gtoToOpts (GetTodoOpts projectId _isOverdue) =
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

isTodoOverdue :: TodoTask -> UTCTime -> Maybe Bool
isTodoOverdue task now = (`isTodoTimeOverdue` now) <$> tdtDateTime task

applyOverdueFilter :: (MonadIO m) => [TodoTask] -> Maybe OverdueFilter -> m [TodoTask]
applyOverdueFilter tasks isOverdue =
  let -- Flag to treat the result of the filter. If overdue is true, then we keep otherwise invert
      flag :: OverdueFilter -> (Bool -> Bool)
      flag ov = if ov then id else not
      filterFn :: UTCTime -> OverdueFilter -> (TodoTask -> Bool)
      filterFn ct ov task = maybe False (flag ov) (isTodoOverdue task ct)
   in do
        currentTime <- liftIO getCurrentTime
        return $
          maybe
            tasks
            (\x -> filter (filterFn currentTime x) tasks)
            isOverdue

class (Monad m) => MonadGetTodos m where
  getTodos :: GetTodoOpts -> m [TodoTask]
  default getTodos :: (MonadRunHttp m, MonadReader t m ,HasTodoistToken t) => GetTodoOpts -> m [TodoTask]
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
