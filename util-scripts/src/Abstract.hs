{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Abstract
  ( MonadAbstract (..),
    MigrationFile (..),
    Environment(..),
    DbEnvironment(..),
    tsFromMigrationFile,
    nameFromMigrationFile,
    fullPathFromMigrationFile,
  )
where

-- ## IO IMPORTS
import qualified Control.Concurrent as C
import Control.Monad.Catch
import Data.List (sortBy)
import Data.String
import qualified Database.PostgreSQL.Simple as PG
import System.Directory
import qualified System.Process as P
import Control.Monad
import qualified Data.Time.Clock.POSIX as T
-- ##

-- | Represents a migration file
data MigrationFile
  = MigrationFile
      -- | The name of the migration
      !String
      -- | The database name
      !String

-- | Extracts the timestamp from a migration file
tsFromMigrationFile :: MigrationFile -> Int
tsFromMigrationFile (MigrationFile s _) = read $ takeWhile (/= '-') s

-- | Extracts the name from a migration file
nameFromMigrationFile :: MigrationFile -> String
nameFromMigrationFile (MigrationFile s _) = (takeWhile (/= '.') . drop 1 . dropWhile (/= '-')) s

-- | Extracts the full path to the migration file
fullPathFromMigrationFile :: MigrationFile -> String
fullPathFromMigrationFile (MigrationFile s db) = "../migrations/" ++ db ++ "/" ++ s

instance Eq MigrationFile where
  (==) a b = tsFromMigrationFile a == tsFromMigrationFile b

instance Ord MigrationFile where
  compare a b = compare (tsFromMigrationFile a) (tsFromMigrationFile b)

data Environment = Local | Production

data DbEnvironment = DbLocal | DbTest | DbProduction

class (MonadMask m) => MonadAbstract conn m | m -> conn where
  -- | Returns a list of database names given a connection
  listDatabases :: conn -> m [String]

  -- | Creates a database given a connection and a database name
  createDatabase :: conn -> String -> m ()

  -- | Opens a tunnel given a database name and an environment
  openTunnel :: String -> DbEnvironment -> m conn

  -- | Closes a tunnel given a connection
  closeTunnel :: conn -> m ()

  -- | Logs a message
  logMessage :: String -> m ()

  -- | Deploys the application part of the stack given an Environment
  deployApplication :: Environment -> m ()

  -- | Deploys the database part of the stack given an Environment
  deployDatabase :: Environment -> m ()

  -- | Returns a list of required databases
  requiredDatabases :: m [String]

  -- | Returns a list of migration files given a database name
  listMigrationFiles :: String -> m [MigrationFile]

  -- | Returns the timestamp of the last migration applied to the database
  lastMigrationTs :: conn -> m (Maybe Int)

  -- | Applies a single migration aginst a connection
  applyMigration :: conn -> MigrationFile -> m ()

  -- | Prepare the migrations tables in the database
  prepMigrations :: conn -> m ()

  -- | Creates a migration file
  -- Given a database name and the name of the migration returns the path to the created file
  createMigrationFile :: String -> String -> m String

  bracketTunnel :: String -> DbEnvironment -> (conn -> m a) -> m a
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

instance MonadAbstract (P.ProcessHandle, PG.Connection) IO where
  logMessage = putStrLn

  deployDatabase env =
    P.callCommand $
      "cd " ++ envInfraDir env ++ "/db terraform init && terraform apply -auto-approve"

  deployApplication env =
    P.callCommand $
      "cd " ++ envInfraDir env ++ "/application terraform init && terraform apply -auto-approve"

  listDatabases (_, c) =
    map PG.fromOnly <$> (PG.query_ c "SELECT datname FROM pg_database" :: IO [PG.Only String])

  createDatabase (_, c) name =
    let statement = fromString $ "CREATE DATABASE " ++ name
     in PG.execute_ c statement >> putStrLn ("Database " ++ name ++ " created")

  openTunnel dbName env =
    let kubeContext = dbEnvKubeContext env
        namespace = dbEnvKubeNamespace env
     in do
          P.callCommand $ "kubectl config use-context " ++ kubeContext
          P.callCommand $ "kubectl config set-context --current --namespace=" ++ namespace
          ph <- P.spawnCommand "kubectl port-forward svc/database 5432:5432"
          -- Wait 3 seconds for the port-forward to be ready
          C.threadDelay 3000000
          conn <- PG.connectPostgreSQL $ fromString $ "postgresql://postgres@localhost:5432/" ++ dbName
          return (ph, conn)

  closeTunnel (ph, _) = P.terminateProcess ph

  requiredDatabases = do
    items <- listDirectory "../migrations"
    areDirs <- mapM (doesDirectoryExist . ("../migrations/" ++)) items
    return [d | (d, m) <- zip items areDirs, m]

  listMigrationFiles dbName =
    sortBy (flip compare) . fmap (`MigrationFile` dbName) <$> listDirectory ("../migrations/" ++ dbName)

  lastMigrationTs (_, c) =
    let statement = "SELECT ts FROM migration ORDER BY ts DESC LIMIT 1"
     in do
          tss <- fmap PG.fromOnly <$> (PG.query_ c statement :: IO [PG.Only Int])
          case tss of
            [] -> return Nothing
            ts : _ -> return $ Just ts

  applyMigration (_, c) migration =
    let name = nameFromMigrationFile migration
        timestamp = (show . tsFromMigrationFile) migration
        filepath = fullPathFromMigrationFile migration
        insertMigrationStatement = "INSERT INTO migration (ts, name) VALUES (" ++ timestamp ++ ", '" ++ name ++ "'); \n"
        fileContents = readFile filepath
        statement = (insertMigrationStatement ++) <$> fileContents
     in statement
          >>= \s -> PG.execute_ c (fromString s) >> putStrLn ("Applied SQL:\n" ++ s ++ "\n")

  prepMigrations (_, c) =  do
    statement <- fromString <$> readFile "../migrations/migration.sql"
    void (PG.execute_ c statement)

  createMigrationFile dbName name =
    let prefix = "../migrations/" ++ dbName ++ "/"
        filepath =
          (prefix ++) . (++ "-" ++ name ++ ".sql") . show <$> T.getPOSIXTime
        content = "-- Add your migration here"
     in filepath >>= \p -> writeFile p content >> return p
