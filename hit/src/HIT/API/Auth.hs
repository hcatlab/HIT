{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}

module HIT.API.Auth where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)
import HIT.API.Users (PublicUser)
import HIT.Types.Identifier (Username)
import HIT.Types.User (APIToken)

-- * signup

-- | timezone defaults to UTC
data SignupRequest = SignupRequest
  { id :: Username,
    email :: Text,
    password :: Text,
    timezone :: Maybe Text
  }
  deriving (Show, Eq, Generic)

instance ToJSON SignupRequest

instance FromJSON SignupRequest

data SignupResponse = SignupResponse
  { token :: APIToken,
    user :: PublicUser
  }
  deriving (Show, Eq, Generic)

instance ToJSON SignupResponse

instance FromJSON SignupResponse

-- * login

data LoginRequest = LoginRequest
  { id :: Username,
    password :: Text
  }
  deriving (Show, Eq, Generic)

instance ToJSON LoginRequest

instance FromJSON LoginRequest

data LoginResponse = LoginResponse
  { token :: APIToken,
    user :: PublicUser
  }
  deriving (Show, Eq, Generic)

instance ToJSON LoginResponse

instance FromJSON LoginResponse
