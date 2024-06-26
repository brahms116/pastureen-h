{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Todos.Domain
  ( HasTodoistToken (..),
    MonadCreateTodo (..),
    MonadDeleteTodo (..),
    MonadGetTodos (..),
    TodoistToken
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

tokenOpts :: (MonadReader t m, HasTodoistToken t) => m (Option 'Https)
tokenOpts = do
  asks ((oAuth2Bearer . TE.encodeUtf8) . getTodoistToken)

class (Monad m) => MonadDeleteTodo m where
  deleteTodo :: TodoId -> m ()
  default deleteTodo :: (MonadRunHttp m, MonadReader t m, HasTodoistToken t) => TodoId -> m ()
  deleteTodo taskId = do
    option <- tokenOpts
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
    option <- tokenOpts
    response <-
      runHttpReq $
        req
          POST
          (https "api.todoist.com" /: "rest" /: "v2" /: "tasks")
          (ReqBodyJson cdtd)
          jsonResponse
          option
    return $ responseBody response

gtpToOpts :: GetTodoParams -> Option 'Https
gtpToOpts (GetTodoParams projectId _isOverdue) =
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

data OverdueStatus = Overdue | NotOverdue | NA deriving (Show, Eq)

classifyOverdue :: TodoTask -> CurrentTime -> OverdueStatus
classifyOverdue task now =
  maybe
    NA
    (\dt -> if isTodoTimeOverdue dt now then Overdue else NotOverdue)
    (tdtDateTime task)

filterToAllowedStatuses :: Maybe OverdueFilter -> [OverdueStatus]
filterToAllowedStatuses Nothing = [Overdue, NotOverdue, NA]
filterToAllowedStatuses (Just True) = [Overdue]
filterToAllowedStatuses (Just False) = [NotOverdue]

applyOverdueFilter :: (MonadIO m) => [TodoTask] -> Maybe OverdueFilter -> m [TodoTask]
applyOverdueFilter tasks isOverdue = do
  currentTime <- liftIO getCurrentTime
  return $ [t | t <- tasks, classifyOverdue t currentTime `elem` filterToAllowedStatuses isOverdue]

class (Monad m) => MonadGetTodos m where
  getTodos :: GetTodoParams -> m [TodoTask]
  default getTodos :: (MonadRunHttp m, MonadReader t m, HasTodoistToken t) => GetTodoParams -> m [TodoTask]
  getTodos opts = do
    option <- (gtpToOpts opts <>) <$> tokenOpts
    response <-
      runHttpReq $
        req
          GET
          (https "api.todoist.com" /: "rest" /: "v2" /: "tasks")
          NoReqBody
          jsonResponse
          option
    applyOverdueFilter (responseBody response) (gtpIsOverdue opts)
