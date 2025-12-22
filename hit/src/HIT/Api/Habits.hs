{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module HIT.Api.Habits where

import Control.Applicative ((<|>))
import Data.Aeson (FromJSON (..), ToJSON (..), object, withObject, (.!=), (.:), (.:?), (.=))
import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import HIT.Types.Deadline (Deadline)
import HIT.Types.Fraction (Fraction)
import HIT.Types.Interval (Interval (Daily, Weekly))
import HIT.Types.Sort (Sort)
import Servant

-- Habits API: unified list + p-parameterized CRUD subroutes

type HabitsApi =
  QueryParam "interval" Interval :> Get '[JSON] [HabitView]
    :<|> "daily" :> HabitApiFor 'Daily
    :<|> "weekly" :> HabitApiFor 'Weekly

type HabitApiFor (p :: Interval) =
  Get '[JSON] [HabitResponse p]
    :<|> ReqBody '[JSON] (CreateHabitRequest p) :> Post '[JSON] (HabitResponse p)
    :<|> Capture "habitId" Text :> Get '[JSON] (HabitResponse p)
    :<|> Capture "habitId" Text :> ReqBody '[JSON] (UpdateHabitRequest p) :> Put '[JSON] (HabitResponse p)
    :<|> Capture "habitId" Text :> Delete '[JSON] NoContent

-- p-parameterized payloads for CRUD
data CreateHabitRequest (p :: Interval) = CreateHabitRequest
  { name :: Text,
    description :: Maybe Text,
    sort :: Sort,
    rate :: Fraction,
    deadline :: Deadline p,
    goalIds :: NonEmpty Text
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
    deadline :: Deadline p,
    goalIds :: NonEmpty Text
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
    deadline :: Deadline p,
    goalIds :: [Text],
    createdAt :: UTCTime,
    modifiedAt :: UTCTime
  }
  deriving (Show, Eq, Generic)

instance ToJSON (HabitResponse p)

instance FromJSON (HabitResponse 'Daily)

instance FromJSON (HabitResponse 'Weekly)

-- Unified list view that includes interval and deadline as a sum
data HabitDeadline
  = DailyDeadline (Deadline 'Daily)
  | WeeklyDeadline (Deadline 'Weekly)
  deriving (Show, Eq, Generic)

data HabitView = HabitView
  { id :: Text,
    interval :: Interval,
    name :: Text,
    description :: Maybe Text,
    sort :: Sort,
    rate :: Fraction,
    deadline :: HabitDeadline,
    goalIds :: [Text],
    createdAt :: UTCTime,
    modifiedAt :: UTCTime
  }
  deriving (Show, Eq, Generic)

instance ToJSON HabitDeadline where
  toJSON (DailyDeadline d) = toJSON d
  toJSON (WeeklyDeadline d) = toJSON d

instance FromJSON HabitDeadline where
  parseJSON v = (DailyDeadline <$> parseJSON v) <|> (WeeklyDeadline <$> parseJSON v)

instance ToJSON HabitView where
  toJSON (HabitView hid hint name desc sort rate deadlineVal goalIds createdAt modifiedAt) =
    let baseFields =
          [ "id" .= hid,
            "interval" .= hint,
            "name" .= name,
            "description" .= desc,
            "sort" .= sort,
            "rate" .= rate,
            "createdAt" .= createdAt,
            "modifiedAt" .= modifiedAt
          ]
        deadlineField = case (hint, deadlineVal) of
          (Daily, DailyDeadline d) -> ["deadline" .= d]
          (Weekly, WeeklyDeadline d) -> ["deadline" .= d]
          (_, other) -> ["deadline" .= toJSON other]
        goalsField = ["goalIds" .= goalIds]
     in object (baseFields <> deadlineField <> goalsField)

instance FromJSON HabitView where
  parseJSON =
    withObject "HabitView" $ \o -> do
      hid <- o .: "id"
      hint <- o .: "interval"
      name <- o .: "name"
      desc <- o .:? "description"
      sort <- o .: "sort"
      rate <- o .: "rate"
      deadlineVal <- case hint of
        Daily -> DailyDeadline <$> o .: "deadline"
        Weekly -> WeeklyDeadline <$> o .: "deadline"
      goalIds <- o .:? "goalIds" .!= []
      createdAt <- o .: "createdAt"
      modifiedAt <- o .: "modifiedAt"
      pure (HabitView hid hint name desc sort rate deadlineVal goalIds createdAt modifiedAt)
