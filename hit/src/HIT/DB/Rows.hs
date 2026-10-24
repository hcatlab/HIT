{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | flat beam rows of every entity and the goal links, decoded by 'HIT.DB.Codec'
module HIT.DB.Rows
  ( UserRowT (..),
    UserRow,
    GoalRowT (..),
    GoalRow,
    PrimaryKey (UserRowId, GoalRowId),
    HabitRowT (..),
    HabitRow,
    IntentionRowT (..),
    IntentionRow,
    GoalLinkT (..),
    GoalLink,
  )
where

import Data.Kind (Type)
import Data.Text (Text)
import Data.Time (TimeOfDay, UTCTime)
import Data.Typeable (Typeable)
import Database.Beam (Beamable, Columnar, Identity, PrimaryKey, Table (..))
import GHC.Generics (Generic)
import HIT.DB.DateRange (DateRange)
import HIT.Types.Identifier (Entity, GoalIdentifier, HabitIdentifier, Identifier, IntentionIdentifier, Username)
import HIT.Types.Interval (Interval)
import HIT.Types.User (APIToken)
import HIT.Types.Weekdays (Weekdays)
import Prelude hiding (id)

-- | a user row is a 'HIT.Types.User.User' as stored
data UserRowT f = UserRow
  { -- | primary key
    id :: Columnar f Username,
    -- | unique ignoring case
    email :: Columnar f Text,
    -- | bcrypt hash of the password
    passwordHash :: Columnar f Text,
    -- | bearer token, unique
    apiToken :: Columnar f APIToken,
    -- | iana zone name
    timezone :: Columnar f Text,
    -- | creation time
    createdAt :: Columnar f UTCTime,
    -- | last update, at least createdAt
    modifiedAt :: Columnar f UTCTime
  }
  deriving (Generic)

-- | a user row
type UserRow = UserRowT Identity

deriving stock instance Show UserRow

deriving stock instance Eq UserRow

instance Beamable UserRowT

instance Table UserRowT where
  data PrimaryKey UserRowT f = UserRowId (Columnar f Username) deriving (Generic)
  primaryKey = UserRowId . (.id)

deriving stock instance Show (PrimaryKey UserRowT Identity)

deriving stock instance Eq (PrimaryKey UserRowT Identity)

instance Beamable (PrimaryKey UserRowT)

-- | a goal row is a 'HIT.Types.Goal.Goal' as stored
data GoalRowT f = GoalRow
  { -- | primary key
    id :: Columnar f GoalIdentifier,
    -- | owner
    user :: PrimaryKey UserRowT f,
    -- | title
    name :: Columnar f Text,
    -- | non-blank
    description :: Columnar f Text,
    -- | positive, unique per user
    number :: Columnar f Integer,
    -- | css color
    color :: Columnar f Text,
    -- | days it runs, unbounded above for open-ended, at least two days when bounded
    active :: Columnar f DateRange,
    -- | creation time
    createdAt :: Columnar f UTCTime,
    -- | last update, at least createdAt
    modifiedAt :: Columnar f UTCTime
  }
  deriving (Generic)

-- | a goal row
type GoalRow = GoalRowT Identity

deriving stock instance Show GoalRow

deriving stock instance Eq GoalRow

instance Beamable GoalRowT

instance Table GoalRowT where
  data PrimaryKey GoalRowT f = GoalRowId (Columnar f GoalIdentifier) deriving (Generic)
  primaryKey = GoalRowId . (.id)

instance Beamable (PrimaryKey GoalRowT)

-- | a habit row is a 'HIT.Types.Habit.Habit' as stored
data HabitRowT f = HabitRow
  { -- | primary key
    id :: Columnar f HabitIdentifier,
    -- | owner
    user :: PrimaryKey UserRowT f,
    -- | title of each occurrence, non-blank
    title :: Columnar f Text,
    -- | target of each occurrence, positive
    target :: Columnar f Double,
    -- | unit of target, e.g. km, nothing for a count
    unit :: Columnar f (Maybe Text),
    -- | daily occurrences span one day, weekly ones one iso week
    interval :: Columnar f Interval,
    -- | days a daily habit occurs on, nothing iff weekly
    weekdays :: Columnar f (Maybe Weekdays),
    -- | local time each occurrence is due by on its due date
    dueTime :: Columnar f (Maybe TimeOfDay),
    -- | days it occurs on, unbounded above for open-ended
    active :: Columnar f DateRange,
    -- | creation time
    createdAt :: Columnar f UTCTime,
    -- | last update, at least createdAt
    modifiedAt :: Columnar f UTCTime
  }
  deriving (Generic)

-- | a habit row
type HabitRow = HabitRowT Identity

deriving stock instance Show HabitRow

deriving stock instance Eq HabitRow

instance Beamable HabitRowT

instance Table HabitRowT where
  data PrimaryKey HabitRowT f = HabitRowId (Columnar f HabitIdentifier) deriving (Generic)
  primaryKey = HabitRowId . (.id)

instance Beamable (PrimaryKey HabitRowT)

-- | an intention row is a 'HIT.Types.Intention.Intention' as stored
data IntentionRowT f = IntentionRow
  { -- | primary key
    id :: Columnar f IntentionIdentifier,
    -- | owner
    user :: PrimaryKey UserRowT f,
    -- | habit it is an occurrence of, nothing for a one-off, unique with dueDate
    habitId :: Columnar f (Maybe HabitIdentifier),
    -- | non-blank
    title :: Columnar f Text,
    -- | amount to reach, positive, 1 for a plain check-off
    target :: Columnar f Double,
    -- | unit of target and progress, e.g. words, nothing for a count
    unit :: Columnar f (Maybe Text),
    -- | amount done so far, non-negative
    progress :: Columnar f Double,
    -- | daily windows span one day, weekly ones one iso week
    interval :: Columnar f Interval,
    -- | days it is due on, one or a monday-to-sunday seven
    window :: Columnar f DateRange,
    -- | local time it is due by on the last day of window
    dueTime :: Columnar f (Maybe TimeOfDay),
    -- | when progress last reached target, nothing iff not done
    doneAt :: Columnar f (Maybe UTCTime),
    -- | creation time
    createdAt :: Columnar f UTCTime,
    -- | last update, at least createdAt
    modifiedAt :: Columnar f UTCTime
  }
  deriving (Generic)

-- | an intention row
type IntentionRow = IntentionRowT Identity

deriving stock instance Show IntentionRow

deriving stock instance Eq IntentionRow

instance Beamable IntentionRowT

instance Table IntentionRowT where
  data PrimaryKey IntentionRowT f = IntentionRowId (Columnar f IntentionIdentifier) deriving (Generic)
  primaryKey = IntentionRowId . (.id)

instance Beamable (PrimaryKey IntentionRowT)

-- | a goal link is a row of habit_goals or intention_goals, saying that its owner serves its goal
type GoalLinkT :: Entity -> (Type -> Type) -> Type
data GoalLinkT entity f = GoalLink
  { -- | the habit or intention
    owner :: Columnar f (Identifier entity),
    -- | the goal it serves
    goal :: Columnar f GoalIdentifier
  }
  deriving (Generic)

-- | a goal link row
type GoalLink entity = GoalLinkT entity Identity

instance Beamable (GoalLinkT entity)

instance (Typeable entity) => Table (GoalLinkT entity) where
  data PrimaryKey (GoalLinkT entity) f = GoalLinkId (Columnar f (Identifier entity)) (Columnar f GoalIdentifier) deriving (Generic)
  primaryKey (GoalLink o g) = GoalLinkId o g

instance Beamable (PrimaryKey (GoalLinkT entity))
