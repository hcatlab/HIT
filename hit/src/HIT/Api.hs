{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeOperators #-}

module HIT.Api where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)
import HIT.Types.User (ApiToken, PublicUser)
import Servant

-- | API type definition for HIT backend
type HITApi =
  "health" :> Get '[JSON] HealthResponse
    :<|> "signup" :> ReqBody '[JSON] SignupRequest :> Post '[JSON] SignupResponse
    :<|> "login" :> ReqBody '[JSON] LoginRequest :> Post '[JSON] LoginResponse
    :<|> AuthProtect "api-token" :> "me" :> Get '[JSON] PublicUser

data SignupRequest = SignupRequest
  { id :: Text,
    email :: Text,
    password :: Text
  }
  deriving (Show, Eq, Generic)

instance ToJSON SignupRequest

instance FromJSON SignupRequest

data SignupResponse = SignupResponse
  { token :: ApiToken,
    user :: PublicUser
  }
  deriving (Show, Eq, Generic)

instance ToJSON SignupResponse

instance FromJSON SignupResponse

data LoginRequest = LoginRequest
  { id :: Text,
    password :: Text
  }
  deriving (Show, Eq, Generic)

instance ToJSON LoginRequest

instance FromJSON LoginRequest

data LoginResponse = LoginResponse
  { token :: ApiToken,
    user :: PublicUser
  }
  deriving (Show, Eq, Generic)

instance ToJSON LoginResponse

instance FromJSON LoginResponse

newtype HealthResponse = HealthResponse
  { status :: String
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)