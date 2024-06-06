{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Todos.TestUtil (TestTodoDomainM (..), withTestTodos, runTestTodoDomainM) where

import Config
import Control.Exception
import Control.Monad.Reader
import Data.Time
import Todos.Domain
import Todos.Types
import Util (MonadRunHttp (..))

newtype TestTodoDomainM a = TestTodoDomainM
  { unTestTodoDomainM :: ReaderT TestConfig IO a
  }
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadReader TestConfig
    )
  deriving anyclass
    ( MonadRunHttp,
      MonadCreateTodo,
      MonadDeleteTodo,
      MonadGetTodos
    )

runTestTodoDomainM :: TestTodoDomainM a -> IO a
runTestTodoDomainM m = do
  config <- defaultTestConfig
  runReaderT (unTestTodoDomainM m) config

createTodos :: [CreateTodoTask] -> IO [TodoTask]
createTodos cts = runTestTodoDomainM $ mapM createTodo cts

deleteTodos :: [TodoTask] -> IO ()
deleteTodos ts =
  runTestTodoDomainM $
    mapM_ deleteTodo $
      tdtId <$> ts

withTestTodos :: (UTCTime -> [CreateTodoTask]) -> ([TodoTask] -> IO ()) -> IO ()
withTestTodos cts a = do
  currentTime <- getCurrentTime
  bracket ((createTodos . cts) currentTime) deleteTodos a
