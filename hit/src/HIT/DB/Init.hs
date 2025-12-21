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
