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
        withTestTodos
          ( \x ->
              [ CreateTodoistTask
                  { ctdtTitle = "Something to do",
                    ctdtProjectId = Nothing,
                    ctdtDateTime = x,
                    ctdtAssigneeId = Nothing
                  }
              ]
          )
          (const $ return ())
          `shouldReturn` ()
    around (withTestTodos testTodos) $ do
      describe "Get Todos" $ do
        it "Should return overdue todos" $ \loadedTodos -> do
          todos <- runTestTodoDomainM $ getTodos $ defaultGetTodoOpts {gtoIsOverdue = Just True}
          length todos `shouldBe` 1
          tdtTitle (head todos) `shouldBe` tdtTitle (head loadedTodos)
        it "Should return non-overdue todos" $ \loadedTodos -> do
          todos <- runTestTodoDomainM $ getTodos $ defaultGetTodoOpts {gtoIsOverdue = Just False}
          length todos `shouldBe` 1
          tdtTitle (head todos) `shouldBe` tdtTitle (head todos)
        it "Should return all the todos" $ \_ -> do
          todos <- runTestTodoDomainM $ getTodos defaultGetTodoOpts
          length todos `shouldBe` 2
