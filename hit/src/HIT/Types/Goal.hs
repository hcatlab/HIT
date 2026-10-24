{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}

module HIT.Types.Goal
  ( Goal (..),
  )
where

import Data.Text (Text)
import Data.Time (Day, UTCTime)
import GHC.Generics (Generic)
import HIT.Types.Identifier (GoalIdentifier, Username)
import HIT.Types.Refined (Description, GoalNumber, Title)
import Prelude hiding (id)

-- | a goal is a long-term objective that is served by habits and intentions
data Goal = Goal
  { -- | primary key
    id :: GoalIdentifier,
    -- | owner
    owner :: Username,
    -- | title
    name :: Title,
    -- | explanation
    description :: Description,
    -- | per-user label, the least positive number unused at creation, unique per user
    number :: GoalNumber,
    -- | css color, e.g. #017290
    color :: Text,
    -- | first day
    startDate :: Day,
    -- | last day, nothing for open-ended, after startDate
    endDate :: Maybe Day,
    -- | creation time
    createdAt :: UTCTime,
    -- | last update, at least createdAt
    modifiedAt :: UTCTime
  }
  deriving stock (Show, Eq, Generic)
