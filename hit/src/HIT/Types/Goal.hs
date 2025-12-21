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
import Database.Beam (Beamable, Columnar, HasSqlEqualityCheck, Identity, PrimaryKey, Table (..))
import GHC.Generics (Generic)
import HIT.Types.UUID ()
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
