{-# LANGUAGE OverloadedStrings #-}

module Todos.DomainSpec (spec) where

import Data.Time
import Test.Hspec
import Todos.Domain
import Todos.TestUtil
import Todos.Types

testTodos :: UTCTime -> [CreateTodoistTask]
testTodos currentTime =
  [ CreateTodoistTask
      { ctdtTitle = "A long time overdue",
        ctdtProjectId = Nothing,
        ctdtDateTime = addUTCTime (-nominalDay) currentTime,
        ctdtAssigneeId = Nothing
      },
    CreateTodoistTask
      { ctdtTitle = "Not overdue",
        ctdtProjectId = Nothing,
        ctdtDateTime = addUTCTime nominalDay currentTime,
        ctdtAssigneeId = Nothing
      }
  ]

spec :: Spec
spec = do
  describe "Todos.Domain" $ do
    describe "withTestTodos" $ do
      it "should not crash" $ do
        currentTime <- getCurrentTime
        withTestTodos
          [ CreateTodoistTask
              { ctdtTitle = "Something to do",
                ctdtProjectId = Nothing,
                ctdtDateTime = currentTime,
                ctdtAssigneeId = Nothing
              }
          ]
          (const $ return ())
          `shouldReturn` ()
