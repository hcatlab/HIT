{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeOperators #-}

module HIT.Api.Goals where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)
import Servant

-- Goals API and payloads

type GoalsApi =
  Get '[JSON] [GoalResponse]
    :<|> ReqBody '[JSON] CreateGoalRequest :> Post '[JSON] GoalResponse
    :<|> Capture "goalId" Text :> Get '[JSON] GoalResponse
    :<|> Capture "goalId" Text :> ReqBody '[JSON] UpdateGoalRequest :> Put '[JSON] GoalResponse
    :<|> Capture "goalId" Text :> Delete '[JSON] NoContent

data CreateGoalRequest = CreateGoalRequest
  { name :: Text,
    description :: Maybe Text
  }
  deriving (Show, Eq, Generic)

instance ToJSON CreateGoalRequest

instance FromJSON CreateGoalRequest

data UpdateGoalRequest = UpdateGoalRequest
  { name :: Text,
    description :: Maybe Text
  }
  deriving (Show, Eq, Generic)

instance ToJSON UpdateGoalRequest

instance FromJSON UpdateGoalRequest

data GoalResponse = GoalResponse
  { id :: Text,
    name :: Text,
    description :: Maybe Text
  }
  deriving (Show, Eq, Generic)

instance ToJSON GoalResponse

instance FromJSON GoalResponse
