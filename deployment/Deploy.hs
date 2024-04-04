{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

import qualified Control.Concurrent as C
import Control.Monad.Catch
import Control.Monad.Reader
import Data.String
import qualified Database.PostgreSQL.Simple as PG
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

  bracketTunnel :: String -> (conn -> m a) -> m a
  bracketTunnel s = bracket (openTunnel s) closeTunnel

data Environment = Local | Production

getInfraDirFromEnv :: Environment -> String
getInfraDirFromEnv Local = "../infrastructure/local"
getInfraDirFromEnv Production = "../infrastructure/production"

data Config = Config
  { cfDatabases :: ![String],
    cfEnvironment :: !Environment
  }

getInfraDirFromConfig :: Config -> String
getInfraDirFromConfig = getInfraDirFromEnv . cfEnvironment

type AppM = ReaderT Config IO

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

  requiredDatabases = asks cfDatabases

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

application :: (MonadInfrastructure conn m) => m ()
application = deployDatabase >> fillMissingDbs >> deployApplication

main :: IO ()
main =
  let config = Config ["nocodb"] Local
   in runReaderT application config
