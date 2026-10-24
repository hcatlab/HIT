{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeOperators #-}

-- | users api and payloads
module HIT.API.Users where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import HIT.Types.Identifier (Username)
import HIT.Types.User (User (..))
import Servant

type UsersAPI =
  Get '[JSON] [UserResponse]
    :<|> ReqBody '[JSON] CreateUserRequest :> Post '[JSON] UserResponse
    :<|> Capture "userId" Username :> Get '[JSON] UserResponse
    :<|> Capture "userId" Username :> ReqBody '[JSON] UpdateUserRequest :> Put '[JSON] UserResponse
    :<|> Capture "userId" Username :> Delete '[JSON] NoContent

data CreateUserRequest = CreateUserRequest
  { id :: Username,
    email :: Text,
    password :: Text
  }
  deriving (Show, Eq, Generic)

instance ToJSON CreateUserRequest

instance FromJSON CreateUserRequest

-- | timezone unchanged if missing
data UpdateUserRequest = UpdateUserRequest
  { email :: Text,
    timezone :: Maybe Text
  }
  deriving (Show, Eq, Generic)

instance ToJSON UpdateUserRequest

instance FromJSON UpdateUserRequest

data UserResponse = UserResponse
  { id :: Username,
    email :: Text,
    timezone :: Text,
    createdAt :: UTCTime,
    modifiedAt :: UTCTime
  }
  deriving (Show, Eq, Generic)

instance ToJSON UserResponse

instance FromJSON UserResponse

-- | a public user is a 'HIT.Types.User.User' without its secrets
data PublicUser = PublicUser
  { -- | username
    id :: Username,
    -- | email address
    email :: Text,
    -- | iana zone name
    timezone :: Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | drop the password hash and token
toPublicUser :: User -> PublicUser
toPublicUser u = PublicUser u.id u.email u.timezone
