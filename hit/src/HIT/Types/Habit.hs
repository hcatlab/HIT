{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module HIT.Types.Habit
  ( Habit,
    HabitT (..),
  )
where

import Data.Text (Text)
import Data.Typeable (Typeable)
import Data.UUID (UUID)
import Database.Beam (Beamable, Columnar, Identity, PrimaryKey, Table (..))
import Database.Beam.Postgres (PgJSON (..))
import GHC.Generics (Generic)
import HIT.Types.Deadline (Deadline)
import HIT.Types.Fraction (Fraction)
import HIT.Types.Interval (Interval)
import HIT.Types.Sort (Sort)
import HIT.Types.User (UserT)
import Prelude hiding (id)

-- Beam table for habits parameterized by interval
data HabitT (p :: Interval) f = Habit
  { id :: Columnar f UUID,
    user :: PrimaryKey UserT f,
    name :: Columnar f Text,
    description :: Columnar f (Maybe Text),
    interval :: Columnar f Interval,
    sort :: Columnar f (PgJSON Sort),
    rate :: Columnar f (PgJSON Fraction),
    deadline :: Columnar f (Deadline p)
  }
  deriving (Generic)

type Habit p = HabitT p Identity

deriving instance Show (Habit p)

deriving instance Eq (Habit p)

instance Beamable (HabitT p)

instance (Typeable p) => Table (HabitT p) where
  data PrimaryKey (HabitT p) f = HabitId (Columnar f UUID) deriving (Generic)
  primaryKey (Habit hid _ _ _ _ _ _ _) = HabitId hid

deriving instance Show (PrimaryKey (HabitT p) Identity)

deriving instance Eq (PrimaryKey (HabitT p) Identity)

instance Beamable (PrimaryKey (HabitT p))

-- Store Fraction and Deadline p as TEXT via JSON encoding
