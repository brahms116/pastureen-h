module Service (deploymentPipeline, fillMissingDbs) where

import Abstract
import Control.Monad.Reader

envDbEnvs :: Environment -> [DbEnvironment]
envDbEnvs Local = [DbLocal, DbTest]
envDbEnvs Production = [DbProduction]

-- | Determines the list of databases to create given the existing and required databases names.
-- Returns the list of databases names to create and a respective log message
dbsToCreate :: [DatabaseName] -> [DatabaseName] -> ([DatabaseName], String)
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

-- | Fills creates the required databases that are missing
fillMissingDbs :: (MonadAbstract conn m) => ReaderT Environment m ()
fillMissingDbs =
  let fill_ c = do
        dbs <- listDatabases c
        required <- requiredDatabases
        let (dbs', report) = dbsToCreate dbs required
        logMessage report
        mapM_ (createDatabase c) dbs'
   in asks envDbEnvs >>= \dbEnvs ->
        lift $ mapM_ (\x -> bracketTunnel "postgres" x fill_) dbEnvs

-- | Migrates a database given a database name and the dbEnvrionment its in
migrateDb :: (MonadAbstract conn m) => DatabaseName -> DbEnvironment -> m ()
migrateDb n e = do
  files <- listMigrationFiles n
  bracketTunnel n e $ migrateDb' files

-- | Migrates a database given a list of migration files and a connection
-- helper function for migrateDb
migrateDb' :: (MonadAbstract conn m) => [MigrationFileRef] -> conn -> m ()
migrateDb' mrfs c = do
  lastTs <- prepMigrations c >> lastMigrationTs c
  mapM_ (applyMigration c) $ filterMigrations mrfs lastTs
  where
    -- | Filters the migrations that are greater than the last migration timestamp
    filterMigrations :: [MigrationFileRef] -> Maybe Int -> [MigrationFileRef]
    filterMigrations ms (Just ts) = takeWhile (\x -> mfrTimestamp x > ts) ms
    filterMigrations ms Nothing = ms

-- | Migrates all the required databases
migrateDbs :: (MonadAbstract conn m) => ReaderT Environment m ()
migrateDbs = do
  dbEnvs <- asks envDbEnvs
  dbs <- lift requiredDatabases
  lift $ mapM_ (uncurry migrateDb) [(db, dbEnv) | db <- dbs, dbEnv <- dbEnvs]

-- | Wraps deployDatbase it in ReaderT
deployDatabase' :: (MonadAbstract conn m) => ReaderT Environment m ()
deployDatabase' = ask >>= lift . deployDatabase

-- | Wraps deployApplication in ReaderT
deployApplication' :: (MonadAbstract conn m) => ReaderT Environment m ()
deployApplication' = ask >>= lift . deployApplication

deploymentPipeline :: (MonadAbstract conn m) => ReaderT Environment m ()
deploymentPipeline = deployDatabase' >> fillMissingDbs >> migrateDbs >> deployApplication'

-- ### COMMANDS

data Command = Deploy !Environment | CreateMigration !String | Plan !Environment

parseCommand :: [String] -> Either String Command
parseCommand ["--help"] = undefined
parseCommand ["deploy", "prod"] = Right $ Deploy Production
parseCommand ["deploy"] = Right $ Deploy Local
parseCommand ["deploy", "local"] = Right $ Deploy Local
parseCommand ["plan", "prod"] = Right $ Plan Production
parseCommand ["plan"] = Right $ Plan Local
parseCommand ["plan", "local"] = Right $ Plan Local
parseCommand ["new-migration", x] = Right $ CreateMigration x
parseCommand _ = Left "Failed to parse command, run with --help for instructions"
