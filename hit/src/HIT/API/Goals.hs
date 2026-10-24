{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeOperators #-}

-- | goals api and payloads
module HIT.API.Goals where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Data.Time (Day, UTCTime)
import GHC.Generics (Generic)
import HIT.Types.Identifier (GoalIdentifier)
import HIT.Types.Refined (Description, GoalNumber, Title)
import Servant hiding (Description)

type GoalsAPI =
  Get '[JSON] [GoalResponse]
    :<|> ReqBody '[JSON] CreateGoalRequest :> Post '[JSON] GoalResponse
    :<|> Capture "goalId" GoalIdentifier :> Get '[JSON] GoalResponse
    :<|> Capture "goalId" GoalIdentifier :> ReqBody '[JSON] UpdateGoalRequest :> Put '[JSON] GoalResponse
    :<|> Capture "goalId" GoalIdentifier :> Delete '[JSON] NoContent

-- | a create goal request
data CreateGoalRequest = CreateGoalRequest
  { -- | title
    name :: Title,
    -- | explanation
    description :: Description,
    -- | css color
    color :: Text,
    -- | nothing for today
    startDate :: Maybe Day,
    -- | nothing for open-ended
    endDate :: Maybe Day
  }
  deriving (Show, Eq, Generic)

instance ToJSON CreateGoalRequest

instance FromJSON CreateGoalRequest

-- | an update goal request
data UpdateGoalRequest = UpdateGoalRequest
  { -- | title
    name :: Title,
    -- | explanation
    description :: Description,
    -- | css color
    color :: Text,
    -- | first day
    startDate :: Day,
    -- | nothing for open-ended
    endDate :: Maybe Day
  }
  deriving (Show, Eq, Generic)

instance ToJSON UpdateGoalRequest

instance FromJSON UpdateGoalRequest

data GoalResponse = GoalResponse
  { id :: GoalIdentifier,
    number :: GoalNumber,
    name :: Title,
    description :: Description,
    color :: Text,
    startDate :: Day,
    endDate :: Maybe Day,
    createdAt :: UTCTime,
    modifiedAt :: UTCTime
  }
  deriving (Show, Eq, Generic)

instance ToJSON GoalResponse

instance FromJSON GoalResponse
