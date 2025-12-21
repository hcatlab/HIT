{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module HIT.Types.Goal
  ( GoalT (..),
    Goal,
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Data.UUID (UUID)
import Data.UUID qualified as UUID
import Database.Beam (Beamable, Columnar, FromBackendRow (..), HasSqlEqualityCheck, Identity, PrimaryKey, Table (..))
import Database.Beam.Backend.SQL (HasSqlValueSyntax (..))
import Database.Beam.Sqlite (Sqlite)
import Database.Beam.Sqlite.Syntax (SqliteValueSyntax)
import GHC.Generics (Generic)
import HIT.Types.User (UserT)
import Prelude hiding (id)

data GoalT f = Goal
  { id :: Columnar f UUID,
    user :: PrimaryKey UserT f,
    name :: Columnar f Text,
    description :: Columnar f (Maybe Text)
  }
  deriving (Generic)

type Goal = GoalT Identity

deriving instance Show Goal

deriving instance Eq Goal

instance Beamable GoalT

instance Table GoalT where
  data PrimaryKey GoalT f = GoalId (Columnar f UUID) deriving (Generic)
  primaryKey (Goal gid _ _ _) = GoalId gid

deriving instance Show (PrimaryKey GoalT Identity)

deriving instance Eq (PrimaryKey GoalT Identity)

instance Beamable (PrimaryKey GoalT)

-- Beam <-> SQLite instances to store UUID as TEXT
instance HasSqlValueSyntax SqliteValueSyntax UUID where
  sqlValueSyntax = sqlValueSyntax . UUID.toText

instance FromBackendRow Sqlite UUID where
  fromBackendRow = do
    t <- fromBackendRow @Sqlite @Text
    case UUID.fromText t of
      Just u -> pure u
      Nothing -> fail "Invalid UUID in DB"

instance HasSqlEqualityCheck Sqlite UUID
