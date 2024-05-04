module Command
  ( Environment (..),
    Command (..),
    parseCommand,
  )
where

import Abstract

data Command = Deploy !Environment | CreateMigration !String | Plan !Environment

parseCommand :: [String] -> Either String Command
parseCommand ["--help"] = undefined
parseCommand ["deploy", "prod"] = Right $ Deploy Production
parseCommand ["deploy"] = Right $ Deploy Local
parseCommand ["deploy", "local"] = Right $ Deploy Local
parseCommand ["new-migration", x] = Right $ CreateMigration x
parseCommand _ = Left "Failed to parse command, run with --help for instructions"
