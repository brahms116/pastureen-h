{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

import qualified Control.Concurrent as C
import Control.Monad.Catch
import Control.Monad.Reader
import Data.List (sortBy)
import Data.String
import qualified Data.Time.Clock.POSIX as T
import qualified Database.PostgreSQL.Simple as PG
import System.Directory
import qualified System.Process as P

class (MonadMask m) => MonadInfrastructure conn m | m -> conn where
  listDatabases :: conn -> m [String]
  createDatabase :: conn -> String -> m ()
  openTunnel :: String -> m conn
  closeTunnel :: conn -> m ()
  logMessage :: String -> m ()
  deployApplication :: m ()
  deployDatabase :: m ()
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

data Environment = Local | Production

getInfraDirFromEnv :: Environment -> String
getInfraDirFromEnv Local = "../infrastructure/local"
getInfraDirFromEnv Production = "../infrastructure/production"

data Config = Config
  { cfEnvironment :: !Environment
  }

getInfraDirFromConfig :: Config -> String
getInfraDirFromConfig = getInfraDirFromEnv . cfEnvironment

type AppM = ReaderT Config IO

data MigrationFile = MigrationFile String String

tsFromMigrationFile :: MigrationFile -> Int
tsFromMigrationFile (MigrationFile s _) = read $ takeWhile (/= '-') s

nameFromMigrationFile :: MigrationFile -> String
nameFromMigrationFile (MigrationFile s _) = (takeWhile (/= '.') . drop 1 . dropWhile (/= '-')) s

fullPathFromMigrationFile :: MigrationFile -> String
fullPathFromMigrationFile (MigrationFile s db) = "../migrations/" ++ db ++ "/" ++ s

instance Eq MigrationFile where
  (==) a b = tsFromMigrationFile a == tsFromMigrationFile b

instance Ord MigrationFile where
  compare a b = compare (tsFromMigrationFile a) (tsFromMigrationFile b)

instance MonadInfrastructure (P.ProcessHandle, PG.Connection) AppM where
  logMessage = liftIO . putStrLn

  deployDatabase =
    asks getInfraDirFromConfig >>= \d ->
      liftIO $
        P.callCommand $
          "cd " ++ d ++ "/db terraform init && terraform apply -auto-approve"

  deployApplication =
    asks getInfraDirFromConfig >>= \d ->
      liftIO $
        P.callCommand $
          "cd " ++ d ++ "/application terraform init && terraform apply -auto-approve"

  listDatabases (_, c) =
    liftIO $
      map PG.fromOnly <$> (PG.query_ c "SELECT datname FROM pg_database" :: IO [PG.Only String])

  createDatabase (_, c) name =
    let statement = fromString $ "CREATE DATABASE " ++ name
     in liftIO $ PG.execute_ c statement >> putStrLn ("Database " ++ name ++ " created")

  openTunnel dbName = do
    liftIO $ do
      P.callCommand "kubectl config set-context --current --namespace=pastureen"
      ph <- P.spawnCommand "kubectl port-forward svc/database 5432:5432"
      -- Wait 3 seconds for the port-forward to be ready
      C.threadDelay 3000000
      conn <- PG.connectPostgreSQL $ fromString $ "postgresql://postgres@localhost:5432/" ++ dbName
      return (ph, conn)

  closeTunnel (ph, _) = liftIO $ P.terminateProcess ph

  requiredDatabases = liftIO $ do
    items <- listDirectory "../migrations"
    areDirs <- mapM (doesDirectoryExist . ("../migrations/" ++)) items
    return [d | (d, m) <- zip items areDirs, m]

  listMigrationFiles dbName =
    liftIO $
      sortBy (flip compare) . fmap (`MigrationFile` dbName) <$> listDirectory ("../migrations/" ++ dbName)

  lastMigrationTs (_, c) =
    let statement = "SELECT ts FROM migration ORDER BY ts DESC LIMIT 1"
     in liftIO $ do
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
     in liftIO $
          statement
            >>= \s -> PG.execute_ c (fromString s) >> putStrLn ("Applied SQL:\n" ++ s ++ "\n")

  prepMigrations (_, c) = liftIO $ do
    statement <- fromString <$> readFile "../migrations/migration.sql"
    void (PG.execute_ c statement)

  createMigrationFile dbName name =
    let prefix = "../migrations/" ++ dbName ++ "/"
        filepath =
          (prefix ++) . (++ "-" ++ name ++ ".sql") . show <$> T.getPOSIXTime
        content = "-- Add your migration here"
     in liftIO $ filepath >>= \p -> writeFile p content >> return p

dbsToCreate :: [String] -> [String] -> ([String], String)
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

fillMissingDbs :: (MonadInfrastructure conn m) => m ()
fillMissingDbs =
  let fill_ c = do
        dbs <- listDatabases c
        required <- requiredDatabases
        let (dbs', report) = dbsToCreate dbs required
        logMessage report
        mapM_ (createDatabase c) dbs'
   in bracketTunnel "postgres" fill_

migrateDb :: (MonadInfrastructure conn m) => String -> m ()
migrateDb dbName = do
  files <- listMigrationFiles dbName
  bracketTunnel dbName $ migrateDb' files

migrateDb' :: (MonadInfrastructure conn m) => [MigrationFile] -> conn -> m ()
migrateDb' migrations c = do
  lastTs <- prepMigrations c >> lastMigrationTs c
  mapM_ (applyMigration c) $ filterMigrations migrations lastTs
  where
    filterMigrations ms (Just ts) = takeWhile (\x -> tsFromMigrationFile x > ts) ms
    filterMigrations ms Nothing = ms

migrateDbs :: (MonadInfrastructure conn m) => m ()
migrateDbs = do
  dbs <- requiredDatabases
  mapM_ migrateDb dbs

application :: (MonadInfrastructure conn m) => m ()
application = deployDatabase >> fillMissingDbs >> migrateDbs >> deployApplication

main :: IO ()
main =
  let config = Config Local
   in runReaderT application config
