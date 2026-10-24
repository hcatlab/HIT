{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

-- | intentions api and payloads
module HIT.API.Intentions where

import Data.Aeson (FromJSON, ToJSON)
import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import Data.Time (Day, TimeOfDay, UTCTime)
import GHC.Generics (Generic)
import HIT.CRUD (CRUDAPI)
import HIT.Types.Identifier (GoalIdentifier, HabitIdentifier, IntentionIdentifier)
import HIT.Types.Interval (Interval)
import HIT.Types.Refined (Progress, Target, Title)

type IntentionsAPI = CRUDAPI "intentionId" IntentionIdentifier IntentionResponse CreateIntentionRequest UpdateIntentionRequest

-- | a create intention request
data CreateIntentionRequest = CreateIntentionRequest
  { -- | what to do
    title :: Title,
    -- | nothing for 'HIT.Types.Refined.defaultTarget'
    target :: Maybe Target,
    -- | e.g. words
    unit :: Maybe Text,
    -- | local time it is due by
    dueTime :: Maybe TimeOfDay,
    -- | startDate and dueDate, nothing for today
    date :: Maybe Day,
    -- | goals of the caller, duplicates ignored
    goalIds :: NonEmpty GoalIdentifier
  }
  deriving (Show, Eq, Generic)

instance ToJSON CreateIntentionRequest

instance FromJSON CreateIntentionRequest

-- | an update intention request
data UpdateIntentionRequest = UpdateIntentionRequest
  { -- | what to do
    title :: Title,
    -- | amount to reach
    target :: Target,
    -- | e.g. words
    unit :: Maybe Text,
    -- | local time it is due by
    dueTime :: Maybe TimeOfDay,
    -- | sets doneAt iff it reaches target
    progress :: Progress,
    -- | goals of the caller, duplicates ignored
    goalIds :: NonEmpty GoalIdentifier
  }
  deriving (Show, Eq, Generic)

instance ToJSON UpdateIntentionRequest

instance FromJSON UpdateIntentionRequest

-- | an intention response
data IntentionResponse = IntentionResponse
  { id :: IntentionIdentifier,
    -- | habit its an occurrence of, nothing for a one-off
    habitId :: Maybe HabitIdentifier,
    title :: Title,
    target :: Target,
    unit :: Maybe Text,
    progress :: Progress,
    -- | daily windows span one day, weekly ones one iso week
    interval :: Interval,
    startDate :: Day,
    dueDate :: Day,
    dueTime :: Maybe TimeOfDay,
    doneAt :: Maybe UTCTime,
    goalIds :: [GoalIdentifier],
    createdAt :: UTCTime,
    modifiedAt :: UTCTime
  }
  deriving (Show, Eq, Generic)

instance ToJSON IntentionResponse

instance FromJSON IntentionResponse
