{-# LANGUAGE OverloadedStrings #-}

module Todos.DomainSpec (spec) where
import Todos.Domain
import Todos.TestUtil
import Data.Time
import Test.Hspec
import Todos.Types

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
