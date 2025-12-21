{-# LANGUAGE OverloadedStrings #-}

module HIT.DB.Init
  ( openDb,
    initDb,
  )
where

import Data.ByteString.Char8 qualified as BS
import Database.PostgreSQL.Simple qualified as PG
import System.Environment (lookupEnv)

-- | Open a PostgreSQL database connection
-- Uses DATABASE_URL env var, defaults to local connection
openDb :: IO PG.Connection
openDb = do
  mDatabaseUrl <- lookupEnv "DATABASE_URL"
  let connStr = case mDatabaseUrl of
        Just url -> url
        Nothing -> "host=localhost port=5432 dbname=hit user=postgres password=postgres"
  putStrLn $ "Connecting to PostgreSQL database..."
  PG.connectPostgreSQL (BS.pack connStr)

-- | Initialize PostgreSQL database schema
initDb :: PG.Connection -> IO ()
initDb conn = do
  putStrLn "Initializing PostgreSQL database schema..."

  -- Enable UUID extension
  PG.execute_
    conn
    "CREATE EXTENSION IF NOT EXISTS \"uuid-ossp\""

  -- Create users table
  PG.execute_
    conn
    "CREATE TABLE IF NOT EXISTS users (id TEXT PRIMARY KEY, email TEXT UNIQUE NOT NULL, password_hash TEXT NOT NULL, api_token TEXT UNIQUE NOT NULL)"

  -- Create goals table
  PG.execute_
    conn
    "CREATE TABLE IF NOT EXISTS goals (id TEXT PRIMARY KEY, \"user\" TEXT NOT NULL REFERENCES users(id) ON DELETE CASCADE, name TEXT NOT NULL, description TEXT)"

  -- Create habits_daily table
  PG.execute_
    conn
    "CREATE TABLE IF NOT EXISTS habits_daily (id TEXT PRIMARY KEY, \"user\" TEXT NOT NULL REFERENCES users(id) ON DELETE CASCADE, name TEXT NOT NULL, description TEXT, sort JSONB NOT NULL, rate JSONB NOT NULL, deadline TEXT NOT NULL)"

  -- Create habits_weekly table
  PG.execute_
    conn
    "CREATE TABLE IF NOT EXISTS habits_weekly (id TEXT PRIMARY KEY, \"user\" TEXT NOT NULL REFERENCES users(id) ON DELETE CASCADE, name TEXT NOT NULL, description TEXT, sort JSONB NOT NULL, rate JSONB NOT NULL, deadline TEXT NOT NULL)"

  -- Create intentions_daily table
  PG.execute_
    conn
    "CREATE TABLE IF NOT EXISTS intentions_daily (id TEXT PRIMARY KEY, \"user\" TEXT NOT NULL REFERENCES users(id) ON DELETE CASCADE, name TEXT NOT NULL, description TEXT, rate JSONB NOT NULL, deadline TEXT NOT NULL)"

  -- Create intentions_weekly table
  PG.execute_
    conn
    "CREATE TABLE IF NOT EXISTS intentions_weekly (id TEXT PRIMARY KEY, \"user\" TEXT NOT NULL REFERENCES users(id) ON DELETE CASCADE, name TEXT NOT NULL, description TEXT, rate JSONB NOT NULL, deadline TEXT NOT NULL)"

  putStrLn "Database schema initialized successfully"
