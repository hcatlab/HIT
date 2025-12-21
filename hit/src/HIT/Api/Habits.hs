{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}

module HIT.Api.Habits where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)
import HIT.Types.Deadline (Deadline)
import HIT.Types.Fraction (Fraction)
import HIT.Types.Interval (Interval (Daily, Weekly))
import HIT.Types.Sort (Sort)
import Servant

-- Habits API and payloads

type HabitsApi =
  "daily" :> HabitApiFor 'Daily
    :<|> "weekly" :> HabitApiFor 'Weekly

type HabitApiFor (p :: Interval) =
  Get '[JSON] [HabitResponse p]
    :<|> ReqBody '[JSON] (CreateHabitRequest p) :> Post '[JSON] (HabitResponse p)
    :<|> Capture "habitId" Text :> Get '[JSON] (HabitResponse p)
    :<|> Capture "habitId" Text :> ReqBody '[JSON] (UpdateHabitRequest p) :> Put '[JSON] (HabitResponse p)
    :<|> Capture "habitId" Text :> Delete '[JSON] NoContent

data CreateHabitRequest (p :: Interval) = CreateHabitRequest
  { name :: Text,
    description :: Maybe Text,
    sort :: Sort,
    rate :: Fraction,
    deadline :: Deadline p
  }
  deriving (Show, Eq, Generic)

instance ToJSON (CreateHabitRequest p)

instance FromJSON (CreateHabitRequest 'Daily)

instance FromJSON (CreateHabitRequest 'Weekly)

data UpdateHabitRequest (p :: Interval) = UpdateHabitRequest
  { name :: Text,
    description :: Maybe Text,
    sort :: Sort,
    rate :: Fraction,
    deadline :: Deadline p
  }
  deriving (Show, Eq, Generic)

instance ToJSON (UpdateHabitRequest p)

instance FromJSON (UpdateHabitRequest 'Daily)

instance FromJSON (UpdateHabitRequest 'Weekly)

data HabitResponse (p :: Interval) = HabitResponse
  { id :: Text,
    name :: Text,
    description :: Maybe Text,
    sort :: Sort,
    rate :: Fraction,
    deadline :: Deadline p
  }
  deriving (Show, Eq, Generic)

instance ToJSON (HabitResponse p)

instance FromJSON (HabitResponse 'Daily)

instance FromJSON (HabitResponse 'Weekly)
