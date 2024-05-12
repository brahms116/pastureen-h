{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}

module Abstract
  ( MonadAbstract (..),
    MigrationFileRef (..),
    Environment (..),
    DbEnvironment (..),
    mfrTimestamp,
    mfrName,
    Config (..),
    defaultConfig,
    AppM,
    DatabaseName,
    MigrationName,
    MigrationDir,
    Path,
    Timestamp,
  )
where

-- ## IO IMPORTS
import qualified Control.Concurrent as C
import Control.Monad
import Control.Monad.Catch
-- ##

import Control.Monad.Reader
import Data.List (sortBy)
import Data.String
import qualified Data.Time.Clock.POSIX as T
import qualified Database.PostgreSQL.Simple as PG
import System.Directory
import qualified System.Process as P

-- | References a migration file
data MigrationFileRef
  = MigrationFileRef
      !MigrationName
      !DatabaseName

type MigrationName = String

type DatabaseName = String

type Path = String

type MigrationDir = Path

type Timestamp = Int

-- | Extracts the timestamp from a migration file reference
mfrTimestamp :: MigrationFileRef -> Timestamp
mfrTimestamp (MigrationFileRef s _) = read $ takeWhile (/= '-') s

-- | Extracts the name of the migration from a migration file reference
mfrName :: MigrationFileRef -> MigrationName
mfrName (MigrationFileRef s _) = (takeWhile (/= '.') . drop 1 . dropWhile (/= '-')) s

-- | Returns the path to a migration file given a migration file and the migration dir
mfrPath :: MigrationFileRef -> MigrationDir -> Path
mfrPath (MigrationFileRef s db) migrationDir = migrationDir ++ "/" ++ db ++ "/" ++ s

instance Eq MigrationFileRef where
  (==) a b = mfrTimestamp a == mfrTimestamp b

instance Ord MigrationFileRef where
  compare a b = compare (mfrTimestamp a) (mfrTimestamp b)

data Environment = Local | Production deriving (Show)

data DbEnvironment = DbLocal | DbTest | DbProduction deriving (Show)

class (MonadMask m) => MonadAbstract conn m | m -> conn where
  -- | Returns a list of database names given a connection
  listDatabases :: conn -> m [DatabaseName]

  -- | Creates a database given a connection and a database name
  createDatabase :: conn -> DatabaseName -> m ()

  -- | Opens a tunnel given a database name and an environment
  openTunnel :: DatabaseName -> DbEnvironment -> m conn

  -- | Closes a tunnel given a connection
  closeTunnel :: conn -> m ()

  -- | Logs a message
  logMessage :: String -> m ()

  -- | Deploys the application part of the stack given an Environment
  deployApplication :: Environment -> m ()

  -- | Deploys the database part of the stack given an Environment
  deployDatabase :: Environment -> m ()

  -- | Returns a list of required databases
  requiredDatabases :: m [DatabaseName]

  -- | Returns a list of migration files given a database name
  listMigrationFiles :: DatabaseName -> m [MigrationFileRef]

  -- | Returns the timestamp of the last migration applied to the database
  lastMigrationTs :: conn -> m (Maybe Timestamp)

  -- | Applies a single migration aginst a connection
  applyMigration :: conn -> MigrationFileRef -> m ()

  -- | Prepare the migrations tables in the database
  prepMigrations :: conn -> m ()

  -- | Creates a migration file
  -- Given a database name and the name of the migration returns the path to the created file
  createMigrationFile :: DatabaseName -> MigrationName -> m Path

  bracketTunnel :: DatabaseName -> DbEnvironment -> (conn -> m a) -> m a
  bracketTunnel s env = bracket (openTunnel s env) closeTunnel

-- ##### IO implementation

envInfraDir :: Environment -> String
envInfraDir Production = "../infrastructure/production"
envInfraDir Local = "../infrastructure/local"

dbEnvKubeNamespace :: DbEnvironment -> String
dbEnvKubeNamespace _ = "pastureen"

dbEnvKubeContext :: DbEnvironment -> String
dbEnvKubeContext DbLocal = "docker-desktop"
dbEnvKubeContext DbTest = "docker-desktop"
dbEnvKubeContext DbProduction = "context-czktpqrhmza"

dbEnvKubeService :: DbEnvironment -> String
dbEnvKubeService DbTest = "test-database"
dbEnvKubeService _ = "database"

data Config = Config
  { cfInfraDirFn :: !(Environment -> String),
    cfKubeNamespaceFn :: !(DbEnvironment -> String),
    cfKubeContextFn :: !(DbEnvironment -> String),
    cfMigrationDir :: !String,
    cfDbEnvKubeServiceFn :: !(DbEnvironment -> String)
  }

defaultConfig :: Config
defaultConfig =
  Config
    { cfInfraDirFn = envInfraDir,
      cfKubeNamespaceFn = dbEnvKubeNamespace,
      cfKubeContextFn = dbEnvKubeContext,
      cfDbEnvKubeServiceFn = dbEnvKubeService,
      cfMigrationDir = "../migrations"
    }

type AppM = ReaderT Config IO

type AppDbConnection = (P.ProcessHandle, PG.Connection)

instance MonadAbstract AppDbConnection AppM where
  logMessage :: String -> AppM ()
  logMessage = lift . putStrLn

  deployDatabase :: Environment -> AppM ()
  deployDatabase env =
    asks cfInfraDirFn >>= \f ->
      lift $
        P.callCommand $
          "cd " ++ f env ++ "/db terraform init && terraform apply -auto-approve"

  deployApplication :: Environment -> AppM ()
  deployApplication env =
    asks cfInfraDirFn >>= \f ->
      lift $
        P.callCommand $
          "cd " ++ f env ++ "/application terraform init && terraform apply -auto-approve"

  listDatabases :: AppDbConnection -> AppM [DatabaseName]
  listDatabases (_, c) =
    lift $ map PG.fromOnly <$> (PG.query_ c "SELECT datname FROM pg_database" :: IO [PG.Only String])

  createDatabase :: AppDbConnection -> DatabaseName -> AppM ()
  createDatabase (_, c) name =
    let statement = fromString $ "CREATE DATABASE " ++ name
     in lift $ PG.execute_ c statement >> putStrLn ("Database " ++ name ++ " created")

  openTunnel :: DatabaseName -> DbEnvironment -> AppM AppDbConnection
  openTunnel d e = do
    kubeContext <- asks (($ e) . cfKubeContextFn)
    namespace <- asks (($ e) . cfKubeNamespaceFn)
    service <- asks (($ e) . cfDbEnvKubeServiceFn)
    lift $ do
      P.callCommand $ "kubectl config use-context " ++ kubeContext
      P.callCommand $ "kubectl config set-context --current --namespace=" ++ namespace
      ph <- P.spawnCommand $ "kubectl port-forward svc/" ++ service ++ " 5432:5432"
      -- Wait 3 seconds for the port-forward to be ready
      C.threadDelay 3000000
      conn <- PG.connectPostgreSQL $ fromString $ "postgresql://postgres@localhost:5432/" ++ d
      return (ph, conn)

  closeTunnel :: AppDbConnection -> AppM ()
  closeTunnel (ph, _) = lift $ P.terminateProcess ph

  requiredDatabases :: AppM [DatabaseName]
  requiredDatabases =
    asks cfMigrationDir >>= \x -> lift $ do
      items <- listDirectory x
      areDirs <- mapM (doesDirectoryExist . ((x ++ "/") ++)) items
      return [d | (d, m) <- zip items areDirs, m]

  listMigrationFiles :: DatabaseName -> AppM [MigrationFileRef]
  listMigrationFiles dbName =
    asks cfMigrationDir >>= \x ->
      lift $
        sortBy (flip compare) . fmap (`MigrationFileRef` dbName) <$> listDirectory (x ++ "/" ++ dbName)

  lastMigrationTs :: AppDbConnection -> AppM (Maybe Timestamp)
  lastMigrationTs (_, c) =
    let statement = "SELECT ts FROM migration ORDER BY ts DESC LIMIT 1"
     in lift $ do
          tss <- fmap PG.fromOnly <$> (PG.query_ c statement :: IO [PG.Only Int])
          case tss of
            [] -> return Nothing
            ts : _ -> return $ Just ts

  applyMigration :: AppDbConnection -> MigrationFileRef -> AppM ()
  applyMigration (_, c) mfr =
    let name = mfrName mfr
        timestamp = (show . mfrTimestamp) mfr
        insertMigrationStatement = "INSERT INTO migration (ts, name) VALUES (" ++ timestamp ++ ", '" ++ name ++ "'); \n"
        fileContents = asks cfMigrationDir >>= \dir -> lift $ readFile $ mfrPath mfr dir
        statement = (insertMigrationStatement ++) <$> fileContents
     in statement
          >>= \s -> lift $ PG.execute_ c (fromString s) >> putStrLn ("Applied SQL:\n" ++ s ++ "\n")

  prepMigrations :: AppDbConnection -> AppM ()
  prepMigrations (_, c) =
    asks cfMigrationDir >>= \x -> lift $ do
      statement <- fromString <$> readFile (x ++ "/migration.sql")
      void (PG.execute_ c statement)

  createMigrationFile :: DatabaseName -> MigrationName -> AppM Path
  createMigrationFile d mn =
    asks cfMigrationDir >>= \x ->
      let prefix = x ++ "/" ++ d ++ "/"
          filepath =
            (prefix ++) . (++ "-" ++ mn ++ ".sql") . show <$> T.getPOSIXTime
          content = "-- Add your migration here"
       in lift $ filepath >>= \p -> writeFile p content >> return p
