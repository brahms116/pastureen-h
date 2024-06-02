{-# LANGUAGE OverloadedStrings #-}

module Todos.Types
  ( TodoistTask (..),
    TodoistTaskId,
    CreateTodoistTask (..),
    GetTodoOpts (..),
    defaultGetTodoOpts
  )
where

import Data.Aeson
import qualified Data.Text as T
import Data.Time.Clock

type TodoistTaskId = T.Text

data TodoistTask = TodoistTask
  { tdtId :: !TodoistTaskId,
    tdtAssigneeId :: !(Maybe String),
    tdtProjectId :: !String,
    tdtTitle :: !String,
    tdtCompleted :: !Bool,
    tdtDateTime :: !UTCTime
  }
  deriving (Show, Eq)

instance FromJSON TodoistTask where
  parseJSON = withObject "Todo" $ \v ->
    TodoistTask
      <$> v .: "id"
      <*> v .:? "assignee_id"
      <*> v .: "project_id"
      <*> v .: "content"
      <*> v .: "is_completed"
      <*> (v .: "due" >>= (.: "datetime"))

instance ToJSON TodoistTask where
  toJSON (TodoistTask id' assignee project title completed dt) =
    object
      [ "id" .= id',
        "asignee_id" .= assignee,
        "project" .= project,
        "content" .= title,
        "is_completed" .= completed,
        "due" .= object ["datetime" .= dt]
      ]

data CreateTodoistTask = CreateTodoistTask
  { ctdtAssigneeId :: !(Maybe String),
    ctdtProjectId :: !(Maybe String),
    ctdtTitle :: !String,
    ctdtDateTime :: !UTCTime
  }
  deriving (Show, Eq)

instance ToJSON CreateTodoistTask where
  toJSON (CreateTodoistTask assignee project title dt) =
    object
      [ "assignee_id" .= assignee,
        "project_id" .= project,
        "content" .= title,
        "due_datetime" .= dt
      ]

data GetTodoOpts = GetTodoOpts
  { gtoProjectId :: !(Maybe String),
    gtoIsOverdue :: !(Maybe Bool)
  }
  deriving (Show, Eq)

defaultGetTodoOpts :: GetTodoOpts
defaultGetTodoOpts = GetTodoOpts Nothing Nothing
