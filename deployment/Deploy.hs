{-# LANGUAGE OverloadedStrings #-}

import qualified Control.Concurrent as C
import Control.Monad.Reader
import Data.String
import qualified Database.PostgreSQL.Simple as PG
import qualified System.Process as P

databases :: [String]
databases = ["nocodb"]

type DatabaseContext a = ReaderT PG.Connection IO a

listDatabases :: DatabaseContext [String]
listDatabases =
  ask >>= \conn ->
    liftIO $
      map PG.fromOnly <$> (PG.query_ conn "SELECT datname FROM pg_database" :: IO [PG.Only String])

createDatabase :: String -> DatabaseContext ()
createDatabase name =
  let statement = fromString $ "CREATE DATABASE " ++ name
   in ask >>= \conn ->
        liftIO $ PG.execute_ conn statement >> putStrLn ("Database " ++ name ++ " created")

syncDatabases :: DatabaseContext ()
syncDatabases = do
  existingDatabases <- listDatabases
  liftIO $ do
    putStrLn "Syncing databases..."
    putStrLn "Existing databases:"
    mapM_ putStrLn $ (" -" ++) <$> existingDatabases
    putStrLn "Required databases:"
    mapM_ putStrLn $ (" -" ++) <$> databases
  mapM_ createDatabase (filter (`notElem` existingDatabases) databases)

main :: IO ()
main = do
  P.callCommand "cd ../infrastructure/local/db && terraform init && terraform apply -auto-approve"
  P.callCommand "kubectl config set-context --current --namespace=pastureen"
  ph <- P.spawnCommand "kubectl port-forward svc/database 5432:5432"
  -- Wait 3 seconds for the port-forward to be ready
  C.threadDelay 3000000
  conn <- PG.connectPostgreSQL "postgresql://postgres@localhost:5432/postgres"
  runReaderT syncDatabases conn
  P.terminateProcess ph
