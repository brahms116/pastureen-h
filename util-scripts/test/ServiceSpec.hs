{-# LANGUAGE OverloadedStrings #-}

module ServiceSpec (spec) where

import Abstract
import Control.Monad.Reader
import qualified Database.PostgreSQL.Simple as PG
import Service (fillMissingDbs)
import Test.Hspec
import Util

assertDummyTable :: PG.Connection -> IO ()
assertDummyTable conn = void $ PG.execute_ conn "SELECT id from dummy"

testFut :: IO Bool
testFut = runDbTest (runReaderT fillMissingDbs Production) "../test-migrations/fill-missing-dbs" >> assertFilledDbs

assertFilledDbs :: IO Bool
assertFilledDbs = do
  _ <- assertDummyTable <$> getConn "database"
  return True

spec :: Spec
spec = do
  around_ withDockerDatabase $ do
    describe "fillMissingDbs" $ do
      it "should work correctly" $ do
        testFut `shouldReturn` True
