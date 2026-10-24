{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

-- | habits api and payloads
module HIT.API.Habits where

import Data.Aeson (FromJSON, ToJSON)
import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import Data.Time (Day, TimeOfDay, UTCTime)
import GHC.Generics (Generic)
import HIT.CRUD (CRUDAPI)
import HIT.Types.Identifier (GoalIdentifier, HabitIdentifier)
import HIT.Types.Interval (Interval)
import HIT.Types.Refined (Target, Title)
import HIT.Types.Weekdays (Weekdays)

type HabitsAPI = CRUDAPI "habitId" HabitIdentifier HabitResponse HabitRequest HabitRequest

-- | a habit request for body of create and update, with fields as in 'HIT.Types.Habit.Habit'
data HabitRequest = HabitRequest
  { -- | title of each occurrence
    title :: Title,
    -- | nothing for 'HIT.Types.Refined.defaultTarget'
    target :: Maybe Target,
    -- | e.g. km
    unit :: Maybe Text,
    -- | daily or weekly
    interval :: Interval,
    -- | non-empty, only when daily, nothing for every day
    weekdays :: Maybe Weekdays,
    -- | local time each occurrence is due by
    dueTime :: Maybe TimeOfDay,
    -- | nothing for today on create, kept on update
    startDate :: Maybe Day,
    -- | at least startDate, nothing for open-ended
    endDate :: Maybe Day,
    -- | goals of the caller, duplicates ignored
    goalIds :: NonEmpty GoalIdentifier
  }
  deriving (Show, Eq, Generic)

instance ToJSON HabitRequest

instance FromJSON HabitRequest

-- | a habit response is a habit with its goal ids, with fields as in 'HIT.Types.Habit.Habit'
data HabitResponse = HabitResponse
  { id :: HabitIdentifier,
    title :: Title,
    target :: Target,
    unit :: Maybe Text,
    interval :: Interval,
    -- | present iff daily
    weekdays :: Maybe Weekdays,
    dueTime :: Maybe TimeOfDay,
    startDate :: Day,
    endDate :: Maybe Day,
    goalIds :: [GoalIdentifier],
    createdAt :: UTCTime,
    modifiedAt :: UTCTime
  }
  deriving (Show, Eq, Generic)

instance ToJSON HabitResponse

instance FromJSON HabitResponse
