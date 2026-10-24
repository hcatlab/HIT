{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module HIT.Types.Habit
  ( Habit (..),
    SomeHabit (..),
  )
where

import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import Data.Time (Day, TimeOfDay, UTCTime)
import Data.Type.Equality (TestEquality (..), (:~:) (..))
import HIT.Types.Identifier (GoalIdentifier, HabitIdentifier, Username)
import HIT.Types.Interval (Interval)
import HIT.Types.Refined (Target, Title)
import HIT.Types.Schedule (Schedule)
import Prelude hiding (id)

-- | a habit is a recurring intention, which generates an intention for every occurrence
type Habit :: Interval -> Type
data Habit interval = Habit
  { -- | primary key
    id :: HabitIdentifier,
    -- | owner
    owner :: Username,
    -- | title of each occurrence
    title :: Title,
    -- | target of each occurrence
    target :: Target,
    -- | unit of target, e.g. km, nothing for a count
    unit :: Maybe Text,
    -- | when it occurs, fixing the interval
    schedule :: Schedule interval,
    -- | local time each occurrence is due by on its due date
    dueTime :: Maybe TimeOfDay,
    -- | no occurrence for days before it
    startDate :: Day,
    -- | no occurrence for days after it, nothing for open-ended, at least startDate
    endDate :: Maybe Day,
    -- | goals it serves, copied to each occurrence
    goals :: NonEmpty GoalIdentifier,
    -- | creation time
    createdAt :: UTCTime,
    -- | last update, at least createdAt
    modifiedAt :: UTCTime
  }

deriving stock instance Show (Habit interval)

deriving stock instance Eq (Habit interval)

-- | some habit is a habit of either interval, as listed or decoded
data SomeHabit where
  SomeHabit :: Habit interval -> SomeHabit

deriving stock instance Show SomeHabit

-- | equal iff the intervals match and the habits are equal
instance Eq SomeHabit where
  SomeHabit a == SomeHabit b = case testEquality a.schedule b.schedule of
    Just Refl -> a == b
    Nothing -> False
