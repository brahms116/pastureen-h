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
    getConn
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
  { oOpenTunnel :: !(Maybe (String -> DbEnvironment -> AppM (P.ProcessHandle, PG.Connection))),
    oCloseTunnel :: !(Maybe ((P.ProcessHandle, PG.Connection) -> AppM ())),
    oMigrationDir :: !String
  }

startDatabase :: IO String
startDatabase = do
  containerId <- P.readProcess "docker" ["run", "-d", "-e", "POSTGRES_PASSWORD=postgres", "-p", "5432:5432", "postgres:13"] ""
  threadDelay 5000000
  return $ init containerId

stopDatabase :: String -> IO ()
stopDatabase containerId = P.callProcess "docker" ["stop", containerId] >> P.callProcess "docker" ["rm", containerId]

withDockerDatabase :: IO () -> IO ()
withDockerDatabase a = bracket startDatabase stopDatabase $ const a

fakeOpenTunnel :: String -> DbEnvironment -> AppM (P.ProcessHandle, PG.Connection)
fakeOpenTunnel db _ = lift $ do
  conn <- getConn db
  ph <- P.spawnCommand "echo 'Connecting to database'"
  return (ph, conn)

getConn :: String -> IO PG.Connection
getConn s = PG.connectPostgreSQL $ fromString $ "postgres://postgres:postgres@localhost:5432/" ++ s

fakeCloseTunnel :: (P.ProcessHandle, PG.Connection) -> AppM ()
fakeCloseTunnel _ = lift $ putStrLn "Closing"

pipelineTestOverrides :: String -> TestOverrides
pipelineTestOverrides dir =
  Overrides
    { oOpenTunnel = Just fakeOpenTunnel,
      oCloseTunnel = Just fakeCloseTunnel,
      oMigrationDir = dir
    }

runDbTest :: ReaderT TestOverrides AppM a -> String -> IO a
runDbTest m migrationDir =
  runReaderT (runReaderT m ov) cf
  where
    deriveConfig :: TestOverrides -> Config
    deriveConfig x = defaultConfig {cfMigrationDir = oMigrationDir x}
    ov = pipelineTestOverrides migrationDir
    cf = deriveConfig ov

instance MonadAbstract (P.ProcessHandle, PG.Connection) (ReaderT TestOverrides AppM) where
  listDatabases = lift . listDatabases

  createDatabase c s = lift $ createDatabase c s

  openTunnel s e = do
    o <- asks oOpenTunnel
    case o of
      Just f -> lift $ f s e
      Nothing -> lift $ openTunnel s e

  closeTunnel c = do
    o <- asks oCloseTunnel
    case o of
      Just f -> lift $ f c
      Nothing -> lift $ closeTunnel c

  logMessage = lift . logMessage

  deployApplication = lift . deployApplication

  deployDatabase = lift . deployDatabase

  requiredDatabases = lift requiredDatabases

  listMigrationFiles = lift . listMigrationFiles

  lastMigrationTs = lift . lastMigrationTs

  applyMigration c m = lift $ applyMigration c m

  prepMigrations c = lift $ prepMigrations c

  createMigrationFile s n = lift $ createMigrationFile s n
