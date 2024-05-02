{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Application
  ( Environment (..),
    AppM,
  )
where

import qualified Control.Concurrent as C
import Control.Monad.Reader
import Data.List (sortBy)
import Data.String
import qualified Data.Time.Clock.POSIX as T
import qualified Database.PostgreSQL.Simple as PG
import Abstract
import System.Directory
import qualified System.Process as P

data Environment = Local | Production

envInfraDir :: Environment -> String
envInfraDir Production = "../infrastructure/production"
envInfraDir Local = "../infrastructure/local"

envKubeNamespace :: Environment -> String
envKubeNamespace _ = "pastureen"

envKubeContext :: Environment -> String
envKubeContext Local = "docker-desktop"
envKubeContext Production = "context-czktpqrhmza"

type AppM = ReaderT Environment IO

instance MonadAbstract (P.ProcessHandle, PG.Connection) AppM where
  logMessage = liftIO . putStrLn

  deployDatabase =
    asks envInfraDir >>= \d ->
      liftIO $
        P.callCommand $
          "cd " ++ d ++ "/db terraform init && terraform apply -auto-approve"

  deployApplication =
    asks envInfraDir >>= \d ->
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
    kubeContext <- asks envKubeContext
    namespace <- asks envKubeNamespace
    liftIO $ do
      P.callCommand $ "kubectl config use-context " ++ kubeContext
      P.callCommand $ "kubectl config set-context --current --namespace=" ++ namespace
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
