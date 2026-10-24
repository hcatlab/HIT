{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module HIT.DB.Migrate
  ( migrate,
    migrations,
  )
where

import Data.ByteString (ByteString)
import Data.FileEmbed (embedDir)
import Data.List (sortOn)
import Database.Beam.Migrate.Simple (VerificationResult (..), verifySchema)
import Database.Beam.Postgres (runBeamPostgres)
import Database.Beam.Postgres.Migrate (migrationBackend)
import Database.PostgreSQL.Simple (Connection, Only (..), execute_, query_, withTransaction)
import Database.PostgreSQL.Simple.Migration
import HIT.DB.Schema (checkedHitDb)

-- | on alpha, if a migration file is edited, it wipes the db instead of failing
resetOnChange :: Bool
resetOnChange = True

-- | sql scripts from hit/migrations, by name
migrations :: [(FilePath, ByteString)]
migrations = sortOn fst $(embedDir "migrations")

-- | apply pending migrations then verify against 'checkedHitDb'
migrate :: Connection -> IO ()
migrate conn = withTransaction conn $ do
  -- serialize concurrent starts
  _ <- query_ conn "SELECT pg_advisory_xact_lock(4835)" :: IO [Only ()]
  res <- run
  case res of
    MigrationError e
      | resetOnChange -> do
          putStrLn ("migrations changed (" <> e <> "), resetting database")
          _ <- execute_ conn "DROP SCHEMA public CASCADE; CREATE SCHEMA public"
          run >>= check
      | otherwise -> check res
    MigrationSuccess -> pure ()
  v <- runBeamPostgres conn (verifySchema migrationBackend checkedHitDb)
  case v of
    VerificationSucceeded -> pure ()
    VerificationFailed ps -> fail ("schema does not match hitDb: " <> show ps)
  where
    opts = defaultOptions {optVerbose = Verbose, optTransactionControl = NoNewTransaction}
    run = runMigration conn opts (MigrationCommands (MigrationInitialization : map (uncurry MigrationScript) migrations))
    check (MigrationError e) = fail ("migration failed: " <> e)
    check MigrationSuccess = pure ()
