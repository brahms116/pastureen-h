{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Todos.Types
  ( TodoTask (..),
    TodoId,
    CreateTodoTask (..),
    GetTodoOpts (..),
    TodoTime (..),
    defaultGetTodoOpts,
  )
where

import Data.Aeson
import Data.Aeson.Types
import qualified Data.Text as T
import Data.Time
import Data.Time.Format.ISO8601

type TodoId = T.Text

data TodoTime = TodoDateOnly !Day | TodoLocal !LocalTime | TodoUTC !UTCTime deriving (Show, Eq)

parseTodoTime :: T.Text -> TodoTime
parseTodoTime t =
  let tString = T.unpack t
      localTime = iso8601ParseM tString :: Maybe LocalTime
      day = iso8601ParseM tString :: Maybe Day
      utcTime = iso8601ParseM tString :: Maybe UTCTime
   in case (localTime, day, utcTime) of
        (Just x, _, _) -> TodoLocal x
        (_, Just x, _) -> TodoDateOnly x
        (_, _, Just x) -> TodoUTC x
        _failure -> error $ "Could not parse time: " ++ tString

fmtTodoTime :: TodoTime -> T.Text
fmtTodoTime x = case x of
  TodoDateOnly y -> T.pack $ formatShow iso8601Format y
  TodoLocal y -> T.pack $ formatShow iso8601Format y
  TodoUTC y -> T.pack $ formatShow iso8601Format y

todoTimePair :: TodoTime -> Pair
todoTimePair t = case t of
  TodoDateOnly _ -> "due_date" .= fmtTodoTime t
  TodoLocal _ -> "due_datetime" .= fmtTodoTime t
  TodoUTC _ -> "due_datetime" .= fmtTodoTime t

data TodoTask = TodoTask
  { tdtId :: !TodoId,
    tdtAssigneeId :: !(Maybe T.Text),
    tdtProjectId :: !T.Text,
    tdtTitle :: !T.Text,
    tdtCompleted :: !Bool,
    tdtDateTime :: !(Maybe TodoTime)
  }
  deriving (Show, Eq)

instance FromJSON TodoTask where
  parseJSON = withObject "Todo" $ \v ->
    TodoTask
      <$> v .: "id"
      <*> v .:? "assignee_id"
      <*> v .: "project_id"
      <*> v .: "content"
      <*> v .: "is_completed"
      <*> ( fmap parseTodoTime
              <$> ( v .:? "due"
                      >>= ( \case
                              Just y ->
                                let dateTime = y .: "datetime"
                                    date = y .: "date"
                                 in dateTime <> date
                              Nothing -> return Nothing
                          )
                  )
          )

data CreateTodoTask = CreateTodoTask
  { ctdtAssigneeId :: !(Maybe T.Text),
    ctdtProjectId :: !(Maybe T.Text),
    ctdtTitle :: !T.Text,
    ctdtDateTime :: !TodoTime
  }
  deriving (Show, Eq)

instance ToJSON CreateTodoTask where
  toJSON (CreateTodoTask assignee project title dt) =
    object
      [ "assignee_id" .= assignee,
        "project_id" .= project,
        "content" .= title,
        todoTimePair dt
      ]

data GetTodoOpts = GetTodoOpts
  { gtoProjectId :: !(Maybe T.Text),
    gtoIsOverdue :: !(Maybe Bool)
  }
  deriving (Show, Eq)

defaultGetTodoOpts :: GetTodoOpts
defaultGetTodoOpts = GetTodoOpts Nothing Nothing
