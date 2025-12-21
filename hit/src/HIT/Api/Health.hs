{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module HIT.Api.Health where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)

newtype HealthResponse = HealthResponse
  { status :: String
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)
