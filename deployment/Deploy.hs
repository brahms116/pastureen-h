{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

import qualified Control.Concurrent as C
import Control.Exception
import Control.Monad.Reader
import Data.String
import qualified Database.PostgreSQL.Simple as PG
import qualified System.Process as P

databases :: [String]
databases = ["nocodb"]

class (Monad m) => MonadDb m where
  listDbs :: m [String]
  createDb :: m ()

class (Monad m) => MonadLogger m where
  logMsg :: (Show a) => a -> m ()

class (Monad m) => MonadInfrastructure m where
  deployDb :: m ()
  deployApplication :: m ()

class (Monad m, Monad db) => MonadDbTunnel db m | m -> db where
  runDB :: db a -> m a

class (Monad m) => HasDbConn m where
  dbConn :: m PG.Connection

newtype Config = Config
  { dbs :: [String]
  }

newtype DbM a = DbM {unDbM :: ReaderT PG.Connection IO a}
  deriving (Functor, Applicative, Monad, MonadIO)

instance HasDbConn DbM where
  dbConn = DbM ask

instance MonadLogger DbM where
  logMsg = DbM . liftIO . print

runDbM :: DbM a -> PG.Connection -> IO a
runDbM (DbM x) = runReaderT x

newtype AppM a = AppM {unApp :: ReaderT Config IO a}
  deriving (Functor, Applicative, Monad, MonadIO)

instance MonadLogger AppM where
  logMsg = AppM . liftIO . print

instance MonadInfrastructure AppM where
  deployApplication = deployApplicationI
  deployDb = deployDbI

deployDbI :: (MonadIO m) => m ()
deployDbI =
  liftIO $
    P.callCommand "cd ../infrastructure/local/db && terraform init && terraform apply -auto-approve"

deployApplicationI :: (MonadIO m) => m ()
deployApplicationI =
  liftIO $
    P.callCommand "cd ../infrastructure/local/application && terraform init && terraform apply -auto-approve"

tunnel :: (MonadIO m) => m (P.ProcessHandle, PG.Connection)
tunnel = liftIO $ do
  ph <- P.spawnCommand "kubectl port-forward svc/database 5432:5432"
  -- Wait 3 seconds for the port-forward to be ready
  C.threadDelay 3000000
  conn <- PG.connectPostgreSQL "postgresql://postgres@localhost:5432/postgres"
  return (ph, conn)

killTunnel :: (MonadIO m) => P.ProcessHandle -> m ()
killTunnel = liftIO . P.terminateProcess

runDBI :: (MonadIO m) => DbM a -> m a
runDBI x = liftIO $ bracket tunnel kill action
  where
    kill (ph, _) = killTunnel ph
    action (_, c) = runDbM x c

instance MonadDbTunnel DbM AppM where
  runDB = runDBI

listDatabases :: (MonadIO m, HasDbConn m) => m [String]
listDatabases =
  dbConn >>= \conn ->
    liftIO $
      map PG.fromOnly <$> (PG.query_ conn "SELECT datname FROM pg_database" :: IO [PG.Only String])

createDatabase :: (MonadIO m, HasDbConn m) => String -> m ()
createDatabase name =
  let statement = fromString $ "CREATE DATABASE " ++ name
   in dbConn >>= \conn ->
        liftIO $ PG.execute_ conn statement >> putStrLn ("Database " ++ name ++ " created")

-- syncDatabases :: DatabaseContext ()
-- syncDatabases = do
--   existingDatabases <- listDatabases
--   liftIO $ do
--     putStrLn "Syncing databases..."
--     putStrLn "Existing databases:"
--     mapM_ putStrLn $ (" -" ++) <$> existingDatabases
--     putStrLn "Required databases:"
--     mapM_ putStrLn $ (" -" ++) <$> databases
--   mapM_ createDatabase (filter (`notElem` existingDatabases) databases)

-- main :: IO ()
-- main = do
--   P.callCommand "cd ../infrastructure/local/db && terraform init && terraform apply -auto-approve"
--   P.callCommand "kubectl config set-context --current --namespace=pastureen"
--   ph <- P.spawnCommand "kubectl port-forward svc/database 5432:5432"
--   -- Wait 3 seconds for the port-forward to be ready
--   C.threadDelay 3000000
--   conn <- PG.connectPostgreSQL "postgresql://postgres@localhost:5432/postgres"
--   runReaderT syncDatabases conn
--   P.terminateProcess ph
