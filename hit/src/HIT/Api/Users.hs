{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeOperators #-}

module HIT.Api.Users where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)
import Servant

-- Users API and payloads

type UsersApi =
  Get '[JSON] [UserResponse]
    :<|> ReqBody '[JSON] CreateUserRequest :> Post '[JSON] UserResponse
    :<|> Capture "userId" Text :> Get '[JSON] UserResponse
    :<|> Capture "userId" Text :> ReqBody '[JSON] UpdateUserRequest :> Put '[JSON] UserResponse
    :<|> Capture "userId" Text :> Delete '[JSON] NoContent

data CreateUserRequest = CreateUserRequest
  { id :: Text,
    email :: Text,
    password :: Text
  }
  deriving (Show, Eq, Generic)

instance ToJSON CreateUserRequest

instance FromJSON CreateUserRequest

data UpdateUserRequest = UpdateUserRequest
  { email :: Text
  }
  deriving (Show, Eq, Generic)

instance ToJSON UpdateUserRequest

instance FromJSON UpdateUserRequest

data UserResponse = UserResponse
  { id :: Text,
    email :: Text
  }
  deriving (Show, Eq, Generic)

instance ToJSON UserResponse

instance FromJSON UserResponse
