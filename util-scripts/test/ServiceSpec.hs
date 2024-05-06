{-# LANGUAGE OverloadedStrings #-}

module ServiceSpec (spec) where

import Abstract
import Control.Concurrent (threadDelay)
import Control.Exception (bracket)
import Control.Monad.Reader
import Data.String
import qualified Database.PostgreSQL.Simple as PG
import Service (fillMissingDbs)
import qualified System.Process as P
import Test.Hspec

startDatabase :: IO String
startDatabase = do
  containerId <- P.readProcess "docker" ["run", "-d", "-e", "POSTGRES_PASSWORD=postgres", "-p", "5432:5432", "postgres:13"] ""
  threadDelay 5000000
  return $ init containerId

stopDatabase :: String -> IO ()
stopDatabase containerId = P.callProcess "docker" ["stop", containerId] >> P.callProcess "docker" ["rm", containerId]

withDatabase :: IO () -> IO ()
withDatabase a = bracket startDatabase stopDatabase $ const a

getConn :: String -> IO PG.Connection
getConn s = PG.connectPostgreSQL $ fromString $ "postgres://postgres:postgres@localhost:5432/" ++ s

assertDummyTable :: PG.Connection -> IO ()
assertDummyTable conn = void $ PG.execute_ conn "SELECT id from dummy"

fakeOpenTunnel :: String -> DbEnvironment -> AppM (P.ProcessHandle, PG.Connection)
fakeOpenTunnel db _ = lift $ do
  conn <- getConn db
  ph <- P.spawnCommand "echo 'Connecting to database'"
  return (ph, conn)

fakeCloseTunnel :: (P.ProcessHandle, PG.Connection) -> AppM ()
fakeCloseTunnel _ = lift $ putStrLn "Closing"

overrides :: TestOverrides
overrides = setOpenTunnel fakeOpenTunnel $ setCloseTunnel fakeCloseTunnel $ defaultOverrides "../test-migrations/fill-missing-dbs"

mergeConfig :: TestOverrides -> Config
mergeConfig ov = defaultConfig {cfMigrationDir = migrationDir ov}

runTest :: ReaderT TestOverrides AppM a -> TestOverrides -> IO a
runTest m ov =
  let cf = mergeConfig ov
   in runReaderT (runReaderT m ov) cf

testFut :: IO Bool
testFut = runTest (runReaderT fillMissingDbs Production) overrides >> assertFilledDbs

assertFilledDbs :: IO Bool
assertFilledDbs = do
  _ <- assertDummyTable <$> getConn "database"
  return True

spec :: Spec
spec = do
  around_ withDatabase $ do
    describe "fillMissingDbs" $ do
      it "should work correctly" $ do
        testFut `shouldReturn` True
