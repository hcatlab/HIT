{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}

module HIT.Api.Intentions where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)
import HIT.Types.Deadline (Deadline)
import HIT.Types.Fraction (Fraction)
import HIT.Types.Interval (Interval (Daily, Weekly))
import Servant

-- Intentions API and payloads

type IntentionsApi =
  "daily" :> IntentionApiFor 'Daily
    :<|> "weekly" :> IntentionApiFor 'Weekly

type IntentionApiFor (p :: Interval) =
  Get '[JSON] [IntentionResponse p]
    :<|> ReqBody '[JSON] (CreateIntentionRequest p) :> Post '[JSON] (IntentionResponse p)
    :<|> Capture "intentionId" Text :> Get '[JSON] (IntentionResponse p)
    :<|> Capture "intentionId" Text :> ReqBody '[JSON] (UpdateIntentionRequest p) :> Put '[JSON] (IntentionResponse p)
    :<|> Capture "intentionId" Text :> Delete '[JSON] NoContent

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
