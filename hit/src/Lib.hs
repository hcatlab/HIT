{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Lib where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Servant

-- | API type definition for HIT backend
type HITApi = "health" :> Get '[JSON] HealthResponse

data HealthResponse = HealthResponse
  { status :: String
  }
  deriving (Show, Eq, Generic)

instance ToJSON HealthResponse

instance FromJSON HealthResponse
