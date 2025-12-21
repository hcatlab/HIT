{-# LANGUAGE TypeApplications #-}

module HIT.Types.UUID where

import Data.Text (Text)
import Data.UUID (UUID)
import Data.UUID qualified as UUID
import Database.Beam.Backend.SQL (HasSqlValueSyntax (..))
import Database.Beam.Sqlite (Sqlite)
import Database.Beam.Sqlite.Syntax (SqliteValueSyntax)
import Database.Beam (FromBackendRow (..), HasSqlEqualityCheck)

-- Shared Beam <-> SQLite instances to store UUID as TEXT
instance HasSqlValueSyntax SqliteValueSyntax UUID where
  sqlValueSyntax = sqlValueSyntax . UUID.toText

instance FromBackendRow Sqlite UUID where
  fromBackendRow = do
    t <- fromBackendRow @Sqlite @Text
    case UUID.fromText t of
      Just u -> pure u
      Nothing -> fail "Invalid UUID in DB"

instance HasSqlEqualityCheck Sqlite UUID
