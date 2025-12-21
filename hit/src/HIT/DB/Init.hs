{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HIT.DB.Init where

import Control.Exception (catch)
import Data.Text (Text)
import Database.SQLite.Simple (Connection)
import Database.SQLite.Simple qualified as Sqlite

openDb :: FilePath -> IO Connection
openDb = Sqlite.open

initDb :: Connection -> IO ()
initDb conn = do
  let expectedUsers = ["id", "email", "password_hash", "api_token"]
  let expectedGoals = ["id", "user", "name", "description"]
  let expectedHabitCols = ["id", "user", "name", "description", "sort", "rate", "deadline"]
  let expectedIntentionCols = ["id", "user", "name", "description", "rate", "deadline"]

  let getExistingCols :: IO [Text]
      getExistingCols =
        ( do
            rows <- Sqlite.query_ conn "PRAGMA table_info(users)" :: IO [(Int, Text, Text, Int, Maybe Text, Int)]
            pure (map (\(_, name_, _, _, _, _) -> name_) rows)
        )
          `catch` (\(_ :: Sqlite.SQLError) -> pure [])

  existing <- getExistingCols
  case existing of
    [] -> pure ()
    cols | cols == expectedUsers -> pure ()
    _ -> Sqlite.execute_ conn "DROP TABLE IF EXISTS users"

  Sqlite.execute_
    conn
    "CREATE TABLE IF NOT EXISTS users \
    \(id TEXT PRIMARY KEY, \
    \email TEXT UNIQUE NOT NULL, \
    \password_hash TEXT NOT NULL, \
    \api_token TEXT UNIQUE NOT NULL)"

  Sqlite.execute_
    conn
    "CREATE TABLE IF NOT EXISTS goals \
    \(id TEXT PRIMARY KEY, \
    \user TEXT NOT NULL, \
    \name TEXT NOT NULL, \
    \description TEXT, \
    \FOREIGN KEY (user) REFERENCES users(id))"

  -- habits (daily)
  let getExistingHabitsDaily :: IO [Text]
      getExistingHabitsDaily =
        ( do
            rows <- Sqlite.query_ conn "PRAGMA table_info(habits_daily)" :: IO [(Int, Text, Text, Int, Maybe Text, Int)]
            pure (map (\(_, name_, _, _, _, _) -> name_) rows)
        )
          `catch` (\(_ :: Sqlite.SQLError) -> pure [])

  existingHabitsDaily <- getExistingHabitsDaily
  case existingHabitsDaily of
    [] -> pure ()
    cols | cols == expectedHabitCols -> pure ()
    _ -> Sqlite.execute_ conn "DROP TABLE IF EXISTS habits_daily"

  Sqlite.execute_
    conn
    "CREATE TABLE IF NOT EXISTS habits_daily \
    \(id TEXT PRIMARY KEY, \
    \user TEXT NOT NULL, \
    \name TEXT NOT NULL, \
    \description TEXT, \
    \sort TEXT NOT NULL, \
    \rate TEXT NOT NULL, \
    \deadline TEXT NOT NULL, \
    \FOREIGN KEY (user) REFERENCES users(id))"

  -- habits (weekly)
  let getExistingHabitsWeekly :: IO [Text]
      getExistingHabitsWeekly =
        ( do
            rows <- Sqlite.query_ conn "PRAGMA table_info(habits_weekly)" :: IO [(Int, Text, Text, Int, Maybe Text, Int)]
            pure (map (\(_, name_, _, _, _, _) -> name_) rows)
        )
          `catch` (\(_ :: Sqlite.SQLError) -> pure [])

  existingHabitsWeekly <- getExistingHabitsWeekly
  case existingHabitsWeekly of
    [] -> pure ()
    cols | cols == expectedHabitCols -> pure ()
    _ -> Sqlite.execute_ conn "DROP TABLE IF EXISTS habits_weekly"

  Sqlite.execute_
    conn
    "CREATE TABLE IF NOT EXISTS habits_weekly \
    \(id TEXT PRIMARY KEY, \
    \user TEXT NOT NULL, \
    \name TEXT NOT NULL, \
    \description TEXT, \
    \sort TEXT NOT NULL, \
    \rate TEXT NOT NULL, \
    \deadline TEXT NOT NULL, \
    \FOREIGN KEY (user) REFERENCES users(id))"

  -- intentions (daily)
  let getExistingIntentionsDaily :: IO [Text]
      getExistingIntentionsDaily =
        ( do
            rows <- Sqlite.query_ conn "PRAGMA table_info(intentions_daily)" :: IO [(Int, Text, Text, Int, Maybe Text, Int)]
            pure (map (\(_, name_, _, _, _, _) -> name_) rows)
        )
          `catch` (\(_ :: Sqlite.SQLError) -> pure [])

  existingIntentionsDaily <- getExistingIntentionsDaily
  case existingIntentionsDaily of
    [] -> pure ()
    cols | cols == expectedIntentionCols -> pure ()
    _ -> Sqlite.execute_ conn "DROP TABLE IF EXISTS intentions_daily"

  Sqlite.execute_
    conn
    "CREATE TABLE IF NOT EXISTS intentions_daily \
    \(id TEXT PRIMARY KEY, \
    \user TEXT NOT NULL, \
    \name TEXT NOT NULL, \
    \description TEXT, \
    \rate TEXT NOT NULL, \
    \deadline TEXT NOT NULL, \
    \FOREIGN KEY (user) REFERENCES users(id))"

  -- intentions (weekly)
  let getExistingIntentionsWeekly :: IO [Text]
      getExistingIntentionsWeekly =
        ( do
            rows <- Sqlite.query_ conn "PRAGMA table_info(intentions_weekly)" :: IO [(Int, Text, Text, Int, Maybe Text, Int)]
            pure (map (\(_, name_, _, _, _, _) -> name_) rows)
        )
          `catch` (\(_ :: Sqlite.SQLError) -> pure [])

  existingIntentionsWeekly <- getExistingIntentionsWeekly
  case existingIntentionsWeekly of
    [] -> pure ()
    cols | cols == expectedIntentionCols -> pure ()
    _ -> Sqlite.execute_ conn "DROP TABLE IF EXISTS intentions_weekly"

  Sqlite.execute_
    conn
    "CREATE TABLE IF NOT EXISTS intentions_weekly \
    \(id TEXT PRIMARY KEY, \
    \user TEXT NOT NULL, \
    \name TEXT NOT NULL, \
    \description TEXT, \
    \rate TEXT NOT NULL, \
    \deadline TEXT NOT NULL, \
    \FOREIGN KEY (user) REFERENCES users(id))"
