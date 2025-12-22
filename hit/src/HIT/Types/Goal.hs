{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module HIT.Types.Goal
  ( GoalT (..),
    Goal,
  )
where

import Data.Text (Text)
import Data.Time (UTCTime)
import Data.UUID (UUID)
import Database.Beam (Beamable, Columnar, Identity, PrimaryKey, Table (..))
import GHC.Generics (Generic)
import HIT.Types.User (UserT)
import Prelude hiding (id)

data GoalT f = Goal
  { id :: Columnar f UUID,
    user :: PrimaryKey UserT f,
    name :: Columnar f Text,
    description :: Columnar f (Maybe Text),
    createdAt :: Columnar f UTCTime,
    modifiedAt :: Columnar f UTCTime
  }
  deriving (Generic)

type Goal = GoalT Identity

deriving instance Show Goal

deriving instance Eq Goal

instance Beamable GoalT

instance Table GoalT where
  data PrimaryKey GoalT f = GoalId (Columnar f UUID) deriving (Generic)
  primaryKey (Goal gid _ _ _ _ _) = GoalId gid

deriving instance Show (PrimaryKey GoalT Identity)

deriving instance Eq (PrimaryKey GoalT Identity)

instance Beamable (PrimaryKey GoalT)
