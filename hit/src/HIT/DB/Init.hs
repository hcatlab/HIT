{-# LANGUAGE OverloadedStrings #-}

module HIT.DB.Init
  ( openDb,
    initDb,
  )
where

import Data.ByteString.Char8 qualified as BS
import Data.Maybe (fromMaybe)
import Data.String (fromString)
import Data.Text (Text)
import Data.Text qualified as T
import Database.PostgreSQL.Simple qualified as PG
import System.Environment (lookupEnv)

-- | Open a PostgreSQL database connection
-- Uses DATABASE_URL env var, defaults to local connection
openDb :: IO PG.Connection
openDb = do
  mDatabaseUrl <- lookupEnv "DATABASE_URL"
  let connStr = fromMaybe "host=localhost port=5432 dbname=hit user=postgres password=postgres" mDatabaseUrl
  putStrLn "Connecting to PostgreSQL database..."
  PG.connectPostgreSQL (BS.pack connStr)

-- | Initialize PostgreSQL database schema
initDb :: PG.Connection -> IO ()
initDb conn = do
  putStrLn "Initializing PostgreSQL database schema..."

  -- Enable UUID extension
  let createUuidExtensionSql :: Text
      createUuidExtensionSql =
        T.unlines
          [ "CREATE EXTENSION IF NOT EXISTS \"uuid-ossp\""
          ]
  _ <- PG.execute_ conn (fromString (T.unpack createUuidExtensionSql))

  -- Create enum type for interval values
  let createIntervalEnumSql :: Text
      createIntervalEnumSql =
        T.unlines
          [ "DO $$",
            "BEGIN",
            "  IF NOT EXISTS (SELECT 1 FROM pg_type WHERE typname = 'interval_kind') THEN",
            "    CREATE TYPE interval_kind AS ENUM ('daily', 'weekly');",
            "  END IF;",
            "END;",
            "$$;"
          ]
  _ <- PG.execute_ conn (fromString (T.unpack createIntervalEnumSql))

  -- Create users table
  let createUsersSql :: Text
      createUsersSql =
        T.unlines
          [ "CREATE TABLE IF NOT EXISTS users (",
            "  id TEXT PRIMARY KEY,",
            "  email TEXT UNIQUE NOT NULL,",
            "  password_hash TEXT NOT NULL,",
            "  api_token TEXT UNIQUE NOT NULL",
            ")"
          ]
  _ <- PG.execute_ conn (fromString (T.unpack createUsersSql))

  -- Create goals table
  let createGoalsSql :: Text
      createGoalsSql =
        T.unlines
          [ "CREATE TABLE IF NOT EXISTS goals (",
            "  id TEXT PRIMARY KEY,",
            "  \"user\" TEXT NOT NULL REFERENCES users(id) ON DELETE CASCADE,",
            "  name TEXT NOT NULL,",
            "  description TEXT",
            ")"
          ]
  _ <- PG.execute_ conn (fromString (T.unpack createGoalsSql))

  -- Create unified habits table
  let createHabitsSql :: Text
      createHabitsSql =
        T.unlines
          [ "CREATE TABLE IF NOT EXISTS habits (",
            "  id TEXT PRIMARY KEY,",
            "  \"user\" TEXT NOT NULL REFERENCES users(id) ON DELETE CASCADE,",
            "  name TEXT NOT NULL,",
            "  description TEXT,",
            "  interval interval_kind NOT NULL,",
            "  sort JSONB NOT NULL,",
            "  rate JSONB NOT NULL,",
            "  deadline TEXT NOT NULL",
            ")"
          ]
  _ <- PG.execute_ conn (fromString (T.unpack createHabitsSql))

  -- Create unified intentions table
  let createIntentionsSql :: Text
      createIntentionsSql =
        T.unlines
          [ "CREATE TABLE IF NOT EXISTS intentions (",
            "  id TEXT PRIMARY KEY,",
            "  \"user\" TEXT NOT NULL REFERENCES users(id) ON DELETE CASCADE,",
            "  name TEXT NOT NULL,",
            "  description TEXT,",
            "  interval interval_kind NOT NULL,",
            "  rate JSONB NOT NULL,",
            "  deadline TEXT NOT NULL",
            ")"
          ]
  _ <- PG.execute_ conn (fromString (T.unpack createIntentionsSql))

  putStrLn "Database schema initialized successfully"
