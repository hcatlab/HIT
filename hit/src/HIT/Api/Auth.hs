{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}

module HIT.Api.Auth where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)
import HIT.Types.User (ApiToken, PublicUser)

-- Signup

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

-- Login

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
