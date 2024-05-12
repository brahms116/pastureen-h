{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}

module Util
  ( TestOverrides (..),
    withDockerDatabase,
    runDbTest,
    getConn,
  )
where

import Abstract
import Control.Concurrent (threadDelay)
import Control.Exception (bracket)
import Control.Monad.Reader
import Data.String
import qualified Database.PostgreSQL.Simple as PG
import qualified System.Process as P

data TestOverrides = Overrides
  { oOpenTunnel :: !(Maybe (String -> DbEnvironment -> AppM AppDbConnection)),
    oCloseTunnel :: !(Maybe (AppDbConnection -> AppM ())),
    oMigrationDir :: !MigrationName
  }

type ContainerId = String

-- | Starts a docker container with a postgres database and returns the container id
startDockerDatabase :: IO ContainerId
startDockerDatabase = do
  containerId <- P.readProcess "docker" ["run", "-d", "-e", "POSTGRES_PASSWORD=postgres", "-p", "5432:5432", "postgres:13"] ""
  threadDelay 5000000
  return $ init containerId

-- | Stops and removes the database container given its id
stopDockerDatabase :: ContainerId -> IO ()
stopDockerDatabase cid =
  P.callProcess "docker" ["stop", cid] >> P.callProcess "docker" ["rm", cid]

-- | Bracket to use with hspec for the around_ hook to start and stop the database container
withDockerDatabase :: IO () -> IO ()
withDockerDatabase a = bracket startDockerDatabase stopDockerDatabase $ const a

-- | Fake open tunnel function which just returns the connection to the docker database
fakeOpenTunnel :: DatabaseName -> DbEnvironment -> AppM AppDbConnection
fakeOpenTunnel db _ = lift $ do
  conn <- getConn db
  ph <- P.spawnCommand "echo 'Connecting to database'"
  return (ph, conn)

-- | Returns a connection to the local docker database given the database name
getConn :: DatabaseName -> IO PG.Connection
getConn s = PG.connectPostgreSQL $ fromString $ "postgres://postgres:postgres@localhost:5432/" ++ s

-- | Fake close tunnel function which just prints "Closing" as there's nothing to close with
-- a local docker database
fakeCloseTunnel :: AppDbConnection -> AppM ()
fakeCloseTunnel _ = lift $ putStrLn "Closing"

-- | Returns a TestOverrides given a migration directory, fit for database testing
dbTestOverrides :: MigrationDir -> TestOverrides
dbTestOverrides dir =
  Overrides
    { oOpenTunnel = Just fakeOpenTunnel,
      oCloseTunnel = Just fakeCloseTunnel,
      oMigrationDir = dir
    }

-- | Runs a database test given a migration directory
runDbTest :: ReaderT TestOverrides AppM a -> MigrationDir -> IO a
runDbTest m md =
  runReaderT (runReaderT m ov) cf
  where
    deriveConfig :: TestOverrides -> Config
    deriveConfig x = defaultConfig {cfMigrationDir = oMigrationDir x}
    ov = dbTestOverrides md
    cf = deriveConfig ov

instance MonadAbstract AppDbConnection (ReaderT TestOverrides AppM) where
  listDatabases = lift . listDatabases

  createDatabase c s = lift $ createDatabase c s

  openTunnel s e = do
    o <- asks oOpenTunnel
    case o of
      Just fn -> lift $ fn s e
      Nothing -> lift $ openTunnel s e

  closeTunnel c = do
    o <- asks oCloseTunnel
    case o of
      Just fn -> lift $ fn c
      Nothing -> lift $ closeTunnel c

  logMessage = lift . logMessage

  deployApplication = lift . deployApplication

  deployDatabase = lift . deployDatabase

  requiredDatabases = lift requiredDatabases

  listMigrationFiles = lift . listMigrationFiles

  lastMigrationTs = lift . lastMigrationTs

  applyMigration c mrf = lift $ applyMigration c mrf

  prepMigrations c = lift $ prepMigrations c

  createMigrationFile s n = lift $ createMigrationFile s n
