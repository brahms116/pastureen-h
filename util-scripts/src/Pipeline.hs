{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}

module Pipeline () where

import Control.Concurrent as C
import Control.Exception
import Data.List (sortBy)
import Data.String
import qualified Database.PostgreSQL.Simple as PG
import Control.Monad
import System.Directory
import qualified System.Process as P

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

-- | Returns a migration file reference given a db name and a file name
mfrFromFileName :: DbName -> FilePath -> MigrationFileRef
mfrFromFileName db fp =
  let ts = read $ takeWhile (/= '-') fp
      n = takeWhile (/= '.') $ drop 1 $ dropWhile (/= '-') fp
   in MigrationFileRef {mfrName = n, mfrTimestamp = ts, mfrDbName = db}

instance Eq MigrationFileRef where
  (==) a b = mfrTimestamp a == mfrTimestamp b

-- Compares two migration file refs by their timestamps
instance Ord MigrationFileRef where
  compare a b = compare (mfrTimestamp a) (mfrTimestamp b)

data Env = Local | Test | Production deriving (Show, Eq)

envInfraDirReal :: Env -> InfraProjectDir
envInfraDirReal Local = "../infrastructure/local"
envInfraDirReal Test = "../infrastructure/test"
envInfraDirReal Production = "../infrastructure/production"

envKubeContext :: Env -> String
envKubeContext Local = "docker-desktop"
envKubeContext Test = "docker-desktop"
envKubeContext Production = "oracle-context"

envKubeNamspace :: Env -> String
envKubeNamspace Local = "pastureen"
envKubeNamspace Test = "pastureen-test"
envKubeNamspace Production = "pastureen"

setKubeContext :: Env -> IO ()
setKubeContext env = do
  P.callCommand $ "kubectl config use-context " ++ envKubeContext env
  P.callCommand $ "kubectl config set-context --current --namespace=" ++ envKubeNamspace env

envRunDbActionFnReal :: Env -> RunDbActionFn a
envRunDbActionFnReal env db action =
  let open :: IO (P.ProcessHandle, DbConnection)
      open = do
        setKubeContext env
        ph <- P.spawnCommand "kubectl port-forward svc/database 5432:5432"
        -- Wait 3 seconds for the port-forward to be ready
        C.threadDelay 3000000
        conn <- PG.connectPostgreSQL $ fromString $ "postgresql://postgres@localhost:5432/" ++ db
        return (ph, conn)
      close :: (P.ProcessHandle, DbConnection) -> IO ()
      close (ph, conn) = do
        PG.close conn
        P.terminateProcess ph
   in bracket open close $ \(_, conn) -> action conn

applicationInfraDir :: FilePath
applicationInfraDir = "/application"

dbInfraDir :: FilePath
dbInfraDir = "/db"

deployTerraform :: FilePath -> IO ()
deployTerraform fp = P.callCommand $ "cd " ++ fp ++ " && terraform init && terraform apply -auto-approve"

-- | Returns a list of database names given a connection
listDatabases :: DbConnection -> IO [DbName]
listDatabases c = map PG.fromOnly <$> (PG.query_ c "SELECT datname FROM pg_database" :: IO [PG.Only String])

-- | Returns a list of migration files given a migration dir and a database name
listMigrationFiles :: MigrationDir -> DbName -> IO [MigrationFileRef]
listMigrationFiles mfd db =
  sortBy (flip compare) . fmap (mfrFromFileName db) <$> listDirectory (mfd ++ "/" ++ db)

-- | Runs a migration given a database connection and a migration file reference and the migration dir
-- returns the executed SQL
runMigration :: DbConnection -> MigrationFileRef -> MigrationDir -> IO String
runMigration c mfr dir =
  let name = mfrName mfr
      timestamp = (show . mfrTimestamp) mfr
      insertMigrationStatement = "INSERT INTO migration (ts, name) VALUES (" ++ timestamp ++ ", '" ++ name ++ "'); \n"
      fileContents = readFile $ mfrPath mfr dir
      statement = (insertMigrationStatement ++) <$> fileContents
   in statement
        >>= \s -> PG.execute_ c (fromString s) >> return s

pathToMigrationTableSql :: FilePath
pathToMigrationTableSql = "../migrations/migration.sql"

-- | Given a connection ensures that the migration table exists
prepMigrationTable :: DbConnection -> IO ()
prepMigrationTable c = do
  statement <- fromString <$> readFile pathToMigrationTableSql
  void (PG.execute_ c statement)

-- | Given the migration directory, returns the list of databases which are required
requiredDatabases :: MigrationDir -> IO [DbName]
requiredDatabases = undefined

-- | Given the migration directory, the migration name, the database name, creates a migration file
-- and returns the path to the file
createMigrationFile :: MigrationDir -> MigrationName -> DbName -> MigrationDir -> IO FilePath
createMigrationFile = undefined

-- | Given the infra project directory, deploys the database infra project
deployDb :: InfraProjectDir -> IO ()
deployDb d = deployTerraform $ d ++ dbInfraDir

-- | Given the infra project directory, deploys the application infra project
deployApplication :: InfraProjectDir -> IO ()
deployApplication d = deployTerraform $ d ++ applicationInfraDir

type RunDbActionFn a = DbName -> (DbConnection -> IO a) -> IO a

-- | Determines the list of databases to create given the existing and required databases names.
-- Returns the list of databases names to create and a respective log message
dbsToCreate :: [DbName] -> [DbName] -> ([DbName], String)
dbsToCreate existing required =
  (missing, msg)
  where
    missing = filter (`notElem` existing) required
    fmtDotpoint x = (" -" ++) <$> x
    msgForCreate = case missing of
      [] -> []
      ds -> "Dbs to create: " : fmtDotpoint ds
    msg =
      unlines $
        "Existing dbs"
          : fmtDotpoint existing
          ++ "Required dbs: "
          : fmtDotpoint required
          ++ msgForCreate

fillMissingDbs :: RunDbActionFn a -> IO ()
fillMissingDbs = undefined

migrateDbs :: RunDbActionFn a -> MigrationDir -> IO ()
migrateDbs = undefined

runPipeline' :: RunDbActionFn a -> MigrationDir -> InfraProjectDir -> IO ()
runPipeline' fn md ifd =
  deployDb ifd
    >> fillMissingDbs fn
    >> migrateDbs fn md
    >> deployApplication ifd

migrationDirReal :: MigrationDir
migrationDirReal = "../migrations"

runPipelineReal :: Env -> IO ()
runPipelineReal env =
  let runDbAction = envRunDbActionFnReal env
      infraDir = envInfraDirReal env
   in runPipeline' runDbAction migrationDirReal infraDir