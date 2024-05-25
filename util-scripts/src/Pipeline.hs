module Pipeline () where

import qualified Database.PostgreSQL.Simple as PG

type MigrationName = String

type DbName = String

type MigrationDir = FilePath

type Timestamp = Int

type DbConnection = PG.Connection

type InfraProjectDir = FilePath

data MigrationFileRef = MigrationFileRef
  { mfrName :: !MigrationName,
    mfrTimestamp :: !Timestamp,
    mfrDbName :: !DbName
  }
  deriving (Show)

-- | Returns the path to a migration file given a migration file and the migration dir
mfrPath :: MigrationFileRef -> MigrationDir -> FilePath
mfrPath
  ( MigrationFileRef
      { mfrName = n,
        mfrTimestamp = ts,
        mfrDbName = db
      }
    )
  migrationDir =
    migrationDir ++ "/" ++ db ++ "/" ++ n ++ "-" ++ show ts ++ ".sql"

instance Eq MigrationFileRef where
  (==) a b = mfrTimestamp a == mfrTimestamp b

-- Compares two migration file refs by their timestamps
-- we flip the order of the arguments to sort in descending order
instance Ord MigrationFileRef where
  compare a b = compare (mfrTimestamp a) (mfrTimestamp b)

data Env = Local | Test | Production deriving (Show, Eq)

listDatabases :: DbConnection -> IO [DbName]
listDatabases = undefined

deployDb :: InfraProjectDir -> IO ()
deployDb = undefined

deployApplication :: InfraProjectDir -> IO ()
deployApplication = undefined
