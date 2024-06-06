{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Todos.TypesSpec (spec) where

import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BLU
import qualified Data.Text.Encoding as TE
import Data.Time
import NeatInterpolation
import Test.Hspec
import TestUtil
import Todos.Types

testTodoistTaskJsonLocal :: BLU.ByteString
testTodoistTaskJsonLocal =
  BLU.fromStrict $
    TE.encodeUtf8
      [trimming|
    {
        "assignee_id": "2671362",
        "is_completed": false,
        "content": "Buy Milk",
        "due": {
            "date": "2016-09-01",
            "is_recurring": false,
            "datetime": "2016-09-01T12:00:00",
            "string": "tomorrow at 12",
            "timezone": "Europe/Moscow"
        },
        "id": "2995104339",
        "project_id": "2203306141",
        "section_id": "7025"
    }
  |]

testTodoistTaskJsonDateOnly :: BLU.ByteString
testTodoistTaskJsonDateOnly =
  BLU.fromStrict $
    TE.encodeUtf8
      [trimming|
    {
        "assignee_id": "2671362",
        "is_completed": false,
        "content": "Buy Milk",
        "due": {
            "date": "2016-09-01",
            "is_recurring": false
        },
        "id": "2995104339",
        "project_id": "2203306141",
        "section_id": "7025"
    }
  |]

testTodoistTaskJson :: BLU.ByteString
testTodoistTaskJson =
  BLU.fromStrict $
    TE.encodeUtf8
      [trimming|
    {
        "creator_id": "2671355",
        "created_at": "2019-12-11T22:36:50.000000Z",
        "assignee_id": "2671362",
        "assigner_id": "2671355",
        "comment_count": 10,
        "is_completed": false,
        "content": "Buy Milk",
        "description": "",
        "due": {
            "date": "2016-09-01",
            "is_recurring": false,
            "datetime": "2016-09-01T12:00:00.000000Z",
            "string": "tomorrow at 12",
            "timezone": "Europe/Moscow"
        },
        "duration": null,
        "id": "2995104339",
        "labels": ["Food", "Shopping"],
        "order": 1,
        "priority": 1,
        "project_id": "2203306141",
        "section_id": "7025",
        "parent_id": "2995104589",
        "url": "https://todoist.com/showTask?id=2995104339"
    }
  |]

expectedTodoistTask :: TodoTask
expectedTodoistTask =
  TodoTask
    { tdtId = "2995104339",
      tdtAssigneeId = Just "2671362",
      tdtProjectId = "2203306141",
      tdtTitle = "Buy Milk",
      tdtCompleted = False,
      tdtDateTime =
        Just $
          TodoUTC $
            UTCTime (fromGregorian 2016 9 1) $
              sinceMidnight midday
    }

expectedTodoistTaskLocal :: TodoTask
expectedTodoistTaskLocal =
  expectedTodoistTask
    { tdtDateTime =
        Just $
          TodoLocal $
            LocalTime (fromGregorian 2016 9 1) midday
    }

expectedTodoistTaskDateOnly :: TodoTask
expectedTodoistTaskDateOnly =
  expectedTodoistTask
    { tdtDateTime =
        Just $
          TodoDateOnly $
            fromGregorian 2016 9 1
    }

testCreateTodoistTask :: CreateTodoTask
testCreateTodoistTask =
  CreateTodoTask
    { ctdtAssigneeId = Just "12345",
      ctdtProjectId = Just "12345",
      ctdtTitle = "Buy Me",
      ctdtDateTime = TodoUTC $ UTCTime (fromGregorian 2016 9 1) $ sinceMidnight midday
    }

expectedCreateTodoistTaskJson :: BLU.ByteString
expectedCreateTodoistTaskJson =
  unformatJsonStr $
    BLU.fromStrict $
      TE.encodeUtf8
        [trimming|
    {
        "assignee_id": "12345",
        "project_id": "12345",
        "content": "Buy Me",
        "due_datetime": "2016-09-01T12:00:00Z"
    }
  |]

spec :: Spec
spec = do
  describe "TodoistTask" $ do
    it "should-deserialize-correctly" $
      eitherDecode testTodoistTaskJson
        `shouldBe` Right expectedTodoistTask
    it "localtime-should-deserialize-correctly" $
      eitherDecode testTodoistTaskJsonLocal
        `shouldBe` Right expectedTodoistTaskLocal
    it "dateOnly-should-deserialize-correctly" $
      eitherDecode testTodoistTaskJsonDateOnly
        `shouldBe` Right expectedTodoistTaskDateOnly

  describe "CreateTodoistTask" $ do
    it "should-serialize-correctly" $
      encode testCreateTodoistTask `shouldBe` expectedCreateTodoistTaskJson
