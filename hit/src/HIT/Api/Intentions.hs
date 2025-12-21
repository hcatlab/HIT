{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module HIT.Api.Intentions where

import Control.Applicative ((<|>))
import Data.Aeson (FromJSON (..), ToJSON (..), object, withObject, (.:), (.:?), (.=))
import Data.Text (Text)
import GHC.Generics (Generic)
import HIT.Types.Deadline (Deadline)
import HIT.Types.Fraction (Fraction)
import HIT.Types.Interval (Interval (Daily, Weekly))
import Servant

-- Intentions API: unified list + p-parameterized CRUD subroutes

type IntentionsApi =
  QueryParam "interval" Interval :> Get '[JSON] [IntentionView]
    :<|> "daily" :> IntentionApiFor 'Daily
    :<|> "weekly" :> IntentionApiFor 'Weekly

type IntentionApiFor (p :: Interval) =
  Get '[JSON] [IntentionResponse p]
    :<|> ReqBody '[JSON] (CreateIntentionRequest p) :> Post '[JSON] (IntentionResponse p)
    :<|> Capture "intentionId" Text :> Get '[JSON] (IntentionResponse p)
    :<|> Capture "intentionId" Text :> ReqBody '[JSON] (UpdateIntentionRequest p) :> Put '[JSON] (IntentionResponse p)
    :<|> Capture "intentionId" Text :> Delete '[JSON] NoContent

-- p-parameterized payloads for CRUD
data CreateIntentionRequest (p :: Interval) = CreateIntentionRequest
  { name :: Text,
    description :: Maybe Text,
    rate :: Fraction,
    deadline :: Deadline p
  }
  deriving (Show, Eq, Generic)

instance ToJSON (CreateIntentionRequest p)

instance FromJSON (CreateIntentionRequest 'Daily)

instance FromJSON (CreateIntentionRequest 'Weekly)

data UpdateIntentionRequest (p :: Interval) = UpdateIntentionRequest
  { name :: Text,
    description :: Maybe Text,
    rate :: Fraction,
    deadline :: Deadline p
  }
  deriving (Show, Eq, Generic)

instance ToJSON (UpdateIntentionRequest p)

instance FromJSON (UpdateIntentionRequest 'Daily)

instance FromJSON (UpdateIntentionRequest 'Weekly)

data IntentionResponse (p :: Interval) = IntentionResponse
  { id :: Text,
    name :: Text,
    description :: Maybe Text,
    rate :: Fraction,
    deadline :: Deadline p
  }
  deriving (Show, Eq, Generic)

instance ToJSON (IntentionResponse p)

instance FromJSON (IntentionResponse 'Daily)

instance FromJSON (IntentionResponse 'Weekly)

-- Unified list view
data IntentionDeadline
  = DailyIntentionDeadline (Deadline 'Daily)
  | WeeklyIntentionDeadline (Deadline 'Weekly)
  deriving (Show, Eq, Generic)

data IntentionView = IntentionView
  { id :: Text,
    interval :: Interval,
    name :: Text,
    description :: Maybe Text,
    rate :: Fraction,
    deadline :: IntentionDeadline
  }
  deriving (Show, Eq, Generic)

instance ToJSON IntentionDeadline where
  toJSON (DailyIntentionDeadline d) = toJSON d
  toJSON (WeeklyIntentionDeadline d) = toJSON d

instance FromJSON IntentionDeadline where
  parseJSON v = (DailyIntentionDeadline <$> parseJSON v) <|> (WeeklyIntentionDeadline <$> parseJSON v)

instance ToJSON IntentionView where
  toJSON (IntentionView iid hint name desc rate deadlineVal) =
    let baseFields =
          [ "id" .= iid,
            "interval" .= hint,
            "name" .= name,
            "description" .= desc,
            "rate" .= rate
          ]
        deadlineField = case (hint, deadlineVal) of
          (Daily, DailyIntentionDeadline d) -> ["deadline" .= d]
          (Weekly, WeeklyIntentionDeadline d) -> ["deadline" .= d]
          (_, other) -> ["deadline" .= toJSON other]
     in object (baseFields <> deadlineField)

instance FromJSON IntentionView where
  parseJSON =
    withObject "IntentionView" $ \o -> do
      iid <- o .: "id"
      hint <- o .: "interval"
      name <- o .: "name"
      desc <- o .:? "description"
      rate <- o .: "rate"
      deadlineVal <- case hint of
        Daily -> DailyIntentionDeadline <$> o .: "deadline"
        Weekly -> WeeklyIntentionDeadline <$> o .: "deadline"
      pure (IntentionView iid hint name desc rate deadlineVal)
