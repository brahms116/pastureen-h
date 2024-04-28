{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}

module Infrastructure (
  MonadInfrastructure (..),
  MigrationFile (..),
  tsFromMigrationFile,
  nameFromMigrationFile,
  fullPathFromMigrationFile,
) where

import Control.Monad.Catch

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

class (MonadMask m) => MonadInfrastructure conn m | m -> conn where
  -- | Returns a list of database names given a connection
  listDatabases :: conn -> m [String]

  -- | Creates a database given a connection and a database name
  createDatabase :: conn -> String -> m ()

  -- | Opens a tunnel given a database name
  openTunnel :: String -> m conn

  -- | Closes a tunnel given a connection
  closeTunnel :: conn -> m ()

  -- | Logs a message
  logMessage :: String -> m ()

  -- | Deploys the application part of the stack
  deployApplication :: m ()

  -- | Deploys the database part of the stack
  deployDatabase :: m ()

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

  bracketTunnel :: String -> (conn -> m a) -> m a
  bracketTunnel s = bracket (openTunnel s) closeTunnel
