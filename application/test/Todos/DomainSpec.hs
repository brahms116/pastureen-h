{-# LANGUAGE OverloadedStrings #-}

module Todos.DomainSpec (spec) where

import Data.Time
import Test.Hspec
import Todos.Domain
import Todos.TestUtil
import Todos.Types

testTodos :: UTCTime -> [CreateTodoTask]
testTodos currentTime =
  [ CreateTodoTask
      { ctdtTitle = "A long time overdue",
        ctdtProjectId = Nothing,
        ctdtDateTime = TodoUTC $ addUTCTime (-nominalDay) currentTime,
        ctdtAssigneeId = Nothing
      },
    CreateTodoTask
      { ctdtTitle = "Not overdue",
        ctdtProjectId = Nothing,
        ctdtDateTime = TodoUTC $ addUTCTime nominalDay currentTime,
        ctdtAssigneeId = Nothing
      }
  ]

spec :: Spec
spec = do
  describe "todos-domain" $ do
    describe "withTestTodos" $ do
      it "should-not-crash-with-utc-time" $ do
        withTestTodos
          ( \x ->
              [ CreateTodoTask
                  { ctdtTitle = "Something to do",
                    ctdtProjectId = Nothing,
                    ctdtDateTime = TodoUTC x,
                    ctdtAssigneeId = Nothing
                  }
              ]
          )
          (const $ return ())
          `shouldReturn` ()
    around (withTestTodos testTodos) $ do
      describe "get-todos" $ do
        it "should-return-overdue-todos" $ \loadedTodos -> do
          todos <- runTestTodoDomainM $ getTodos $ defaultGetTodoParams {gtpIsOverdue = Just True}
          length todos `shouldBe` 1
          tdtTitle (head todos) `shouldBe` tdtTitle (head loadedTodos)
        it "should-return-non-overdue-todos" $ \loadedTodos -> do
          todos <- runTestTodoDomainM $ getTodos $ defaultGetTodoParams {gtpIsOverdue = Just False}
          length todos `shouldBe` 1
          tdtTitle (head todos) `shouldBe` tdtTitle (loadedTodos !! 1)
        it "should-return-all-the-todos" $ \_ -> do
          todos <- runTestTodoDomainM $ getTodos defaultGetTodoParams
          length todos `shouldBe` 2
