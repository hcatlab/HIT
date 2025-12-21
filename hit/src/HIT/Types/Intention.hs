{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module HIT.Types.Intention
  ( Intention,
    IntentionT (..),
  )
where

import Data.Text (Text)
import Data.Typeable (Typeable)
import Data.UUID (UUID)
import Database.Beam (Beamable, Columnar, FromBackendRow (..), Identity, PrimaryKey, Table (..))
import GHC.Generics (Generic)
import HIT.Types.Deadline (Deadline)
import HIT.Types.Fraction (Fraction)
import HIT.Types.Interval (Interval)
import HIT.Types.UUID ()
import HIT.Types.User (UserT)
import Prelude hiding (id)

-- Beam table for intentions parameterized by interval
data IntentionT (p :: Interval) f = Intention
  { id :: Columnar f UUID,
    user :: PrimaryKey UserT f,
    name :: Columnar f Text,
    description :: Columnar f (Maybe Text),
    rate :: Columnar f Fraction,
    deadline :: Columnar f (Deadline p)
  }
  deriving (Generic)

type Intention p = IntentionT p Identity

deriving instance Show (Intention p)

deriving instance Eq (Intention p)

instance Beamable (IntentionT p)

instance (Typeable p) => Table (IntentionT p) where
  data PrimaryKey (IntentionT p) f = IntentionId (Columnar f UUID) deriving (Generic)
  primaryKey (Intention iid _ _ _ _ _) = IntentionId iid

deriving instance Show (PrimaryKey (IntentionT p) Identity)

deriving instance Eq (PrimaryKey (IntentionT p) Identity)

instance Beamable (PrimaryKey (IntentionT p))
