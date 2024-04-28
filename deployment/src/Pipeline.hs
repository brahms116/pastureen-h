module Pipeline (deploymentPipeline) where

import Infrastructure

-- | Determines the list of databases to create given the existing and required databases names.
-- Returns the list of databases names to create and a respective log message
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

-- | Fills creates the required databases that are missing
fillMissingDbs :: (MonadInfrastructure conn m) => m ()
fillMissingDbs =
  let fill_ c = do
        dbs <- listDatabases c
        required <- requiredDatabases
        let (dbs', report) = dbsToCreate dbs required
        logMessage report
        mapM_ (createDatabase c) dbs'
   in bracketTunnel "postgres" fill_

-- | Migrates a database given a database name
migrateDb :: (MonadInfrastructure conn m) => String -> m ()
migrateDb dbName = do
  files <- listMigrationFiles dbName
  bracketTunnel dbName $ migrateDb' files

-- | Migrates a database given a list of migration files and a connection
-- helper function for migrateDb
migrateDb' :: (MonadInfrastructure conn m) => [MigrationFile] -> conn -> m ()
migrateDb' migrations c = do
  lastTs <- prepMigrations c >> lastMigrationTs c
  mapM_ (applyMigration c) $ filterMigrations migrations lastTs
  where
    filterMigrations ms (Just ts) = takeWhile (\x -> tsFromMigrationFile x > ts) ms
    filterMigrations ms Nothing = ms

-- | Migrates all the required databases
migrateDbs :: (MonadInfrastructure conn m) => m ()
migrateDbs = do
  dbs <- requiredDatabases
  mapM_ migrateDb dbs

deploymentPipeline :: (MonadInfrastructure conn m) => m ()
deploymentPipeline = deployDatabase >> fillMissingDbs >> migrateDbs >> deployApplication
