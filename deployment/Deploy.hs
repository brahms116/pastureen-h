{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

import qualified Control.Concurrent as C
import Control.Monad.Catch
import Control.Monad.Reader
import Data.String
import qualified Database.PostgreSQL.Simple as PG
import qualified System.Process as P

databases :: [String]
databases = ["nocodb"]

class (MonadMask m) => MonadInfrastructure conn m | m -> conn where
  listDatabases :: conn -> m [String]
  createDatabase :: conn -> String -> m ()
  openTunnel :: m conn
  closeTunnel :: conn -> m ()
  logMessage :: String -> m ()
  deployApplication :: m ()
  deployDatabase :: m ()
  requiredDatabases :: m [String]

  bracketTunnel :: (conn -> m a) -> m a
  bracketTunnel = bracket openTunnel closeTunnel

newtype Config = Config {cfDatabases :: [String]}

-- newtype AppM a = AppM {unApp :: ReaderT Config IO a}
--   deriving (Functor, Applicative, Monad, MonadIO, MonadThrow, MonadCatch, MonadMask)
--

type AppM = ReaderT Config IO

instance MonadInfrastructure (P.ProcessHandle, PG.Connection) AppM where
  logMessage = liftIO . putStrLn

  deployDatabase =
    liftIO $
      P.callCommand "cd ../infrastructure/local/db && terraform init && terraform apply -auto-approve"

  deployApplication =
    liftIO $
      P.callCommand "cd ../infrastructure/local/application && terraform init && terraform apply -auto-approve"

  listDatabases (_, c) =
    liftIO $
      map PG.fromOnly <$> (PG.query_ c "SELECT datname FROM pg_database" :: IO [PG.Only String])

  createDatabase (_, c) name =
    let statement = fromString $ "CREATE DATABASE " ++ name
     in liftIO $ PG.execute_ c statement >> putStrLn ("Database " ++ name ++ " created")

  openTunnel = do
    liftIO $ do
      P.callCommand "kubectl config set-context --current --namespace=pastureen"
      ph <- P.spawnCommand "kubectl port-forward svc/database 5432:5432"
      -- Wait 3 seconds for the port-forward to be ready
      C.threadDelay 3000000
      conn <- PG.connectPostgreSQL "postgresql://postgres@localhost:5432/postgres"
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
         in do 
          logMessage report
          mapM_ (createDatabase c) dbs'
   in bracketTunnel fill_

main :: IO ()
main = putStrLn "Hello, Haskell!"
