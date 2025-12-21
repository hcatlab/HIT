{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module HIT.Types.Habit
  ( Habit,
    HabitT (..),
  )
where

import Data.Aeson (eitherDecode, encode)
import Data.ByteString.Lazy qualified as LBS
import Data.Text (Text)
import Data.Text.Encoding qualified as Text
import Data.Typeable (Typeable)
import Data.UUID (UUID)
import Database.Beam (Beamable, Columnar, FromBackendRow (..), Identity, PrimaryKey, Table (..))
import Database.Beam.Backend.SQL (HasSqlValueSyntax (..))
import Database.Beam.Sqlite (Sqlite)
import Database.Beam.Sqlite.Syntax (SqliteValueSyntax)
import GHC.Generics (Generic)
import HIT.Types.Deadline (Deadline)
import HIT.Types.Fraction (Fraction)
import HIT.Types.Interval (Interval)
import HIT.Types.Sort (Sort)
import HIT.Types.UUID ()
import HIT.Types.User (UserT)
import Prelude hiding (id)

-- Beam table for habits parameterized by interval
data HabitT (p :: Interval) f = Habit
  { id :: Columnar f UUID,
    user :: PrimaryKey UserT f,
    name :: Columnar f Text,
    description :: Columnar f (Maybe Text),
    sort :: Columnar f Sort,
    rate :: Columnar f Fraction,
    deadline :: Columnar f (Deadline p)
  }
  deriving (Generic)

type Habit p = HabitT p Identity

deriving instance Show (Habit p)

deriving instance Eq (Habit p)

instance Beamable (HabitT p)

instance (Typeable p) => Table (HabitT p) where
  data PrimaryKey (HabitT p) f = HabitId (Columnar f UUID) deriving (Generic)
  primaryKey (Habit hid _ _ _ _ _ _) = HabitId hid

deriving instance Show (PrimaryKey (HabitT p) Identity)

deriving instance Eq (PrimaryKey (HabitT p) Identity)

instance Beamable (PrimaryKey (HabitT p))

-- Store Sort, Fraction, and Deadline p as TEXT via JSON encoding
instance HasSqlValueSyntax SqliteValueSyntax Sort where
  sqlValueSyntax = sqlValueSyntax . Text.decodeUtf8 . LBS.toStrict . encode

instance FromBackendRow Sqlite Sort where
  fromBackendRow = do
    t <- fromBackendRow @Sqlite @Text
    case eitherDecode (LBS.fromStrict (Text.encodeUtf8 t)) of
      Right s -> pure s
      Left _ -> fail "Invalid sort"
