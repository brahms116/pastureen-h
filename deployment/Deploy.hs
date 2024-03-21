{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

import qualified Control.Concurrent as C
import Control.Monad.Catch
import Control.Monad.Reader
import Data.String
import qualified Database.PostgreSQL.Simple as PG
import qualified System.Process as P

class (Monad m) => MonadDb m where
  listDbs :: m [String]
  createDb :: String -> m ()

class (Monad m) => MonadInfrastructure m where
  deployDb :: m ()
  deployApplication :: m ()
  logMsg :: (Show a) => a -> m ()

class (MonadThrow m, Monad db) => MonadDbTunnel ph conn db m | m -> db, m -> ph, m -> conn where
  openTunnel :: m (ph, conn)
  closeTunnel :: ph -> m ()
  runDb :: db a -> conn -> m a

class (Monad m) => HasDbConn m where
  dbConn :: m PG.Connection

newtype Config = Config
  { dbs :: [String]
  }

newtype DbM a = DbM (ReaderT PG.Connection IO a)
  deriving (Functor, Applicative, Monad, MonadIO)

instance HasDbConn DbM where
  dbConn = DbM ask

instance MonadDb DbM where
  createDb = createDbI
  listDbs = listDbsI

listDbsI :: (MonadIO m, HasDbConn m) => m [String]
listDbsI =
  dbConn >>= \conn ->
    liftIO $
      map PG.fromOnly <$> (PG.query_ conn "SELECT datname FROM pg_database" :: IO [PG.Only String])

createDbI :: (MonadIO m, HasDbConn m) => String -> m ()
createDbI name =
  let statement = fromString $ "CREATE DATABASE " ++ name
   in dbConn >>= \conn ->
        liftIO $ PG.execute_ conn statement >> putStrLn ("Database " ++ name ++ " created")

runDbM :: DbM a -> PG.Connection -> IO a
runDbM (DbM x) = runReaderT x

newtype AppM a = AppM {unApp :: ReaderT Config IO a}
  deriving (Functor, Applicative, Monad, MonadIO, MonadThrow)

runAppM :: AppM a -> Config -> IO a
runAppM x = runReaderT (unApp x)

instance MonadReader Config AppM where
  ask = AppM ask
  local f (AppM x) = AppM (local f x)

instance MonadInfrastructure AppM where
  deployApplication = deployApplicationI
  deployDb = deployDbI
  logMsg = AppM . liftIO . print

deployDbI :: (MonadIO m) => m ()
deployDbI =
  liftIO $
    P.callCommand "cd ../infrastructure/local/db && terraform init && terraform apply -auto-approve"

deployApplicationI :: (MonadIO m) => m ()
deployApplicationI =
  liftIO $
    P.callCommand "cd ../infrastructure/local/application && terraform init && terraform apply -auto-approve"

instance MonadDbTunnel P.ProcessHandle PG.Connection DbM AppM where
  openTunnel = openTunnelI
  closeTunnel = closeTunnelI
  runDb = runDbI

openTunnelI :: (MonadIO m) => m (P.ProcessHandle, PG.Connection)
openTunnelI = liftIO $ do
  ph <- P.spawnCommand "kubectl port-forward svc/database 5432:5432"
  -- Wait 3 seconds for the port-forward to be ready
  C.threadDelay 3000000
  conn <- PG.connectPostgreSQL "postgresql://postgres@localhost:5432/postgres"
  return (ph, conn)

closeTunnelI :: (MonadIO m) => P.ProcessHandle -> m ()
closeTunnelI = liftIO . P.terminateProcess

runDbI :: (MonadIO m) => DbM a -> PG.Connection -> m a
runDbI x = liftIO . runDbM x

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

createMissingDbs :: (MonadDb db, MonadReader Config m, MonadInfrastructure m, MonadDbTunnel ph conn db m) => m ()
createMissingDbs = do
  logMsg ("Syncing Databases: " :: String)
  (h, con) <- openTunnel
  existing <- runDb listDbs con
  required <- asks dbs
  let (toCreate, report) = dbsToCreate existing required
  logMsg report
  logMsg ("Creating Databases: " :: String)
  mapM_ (\x -> runDb (createDb x) con) toCreate
  closeTunnel h

application :: (MonadDb db, MonadReader Config m, MonadInfrastructure m, MonadDbTunnel ph conn db m) => m ()
application = deployDb >> createMissingDbs >> deployApplication

main :: IO ()
main = runAppM application Config {dbs = ["nocodb"]}
