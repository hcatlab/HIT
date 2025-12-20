{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module HIT.Types.User
  ( UserT (..),
    User,
    PublicUser (..),
    toPublicUser,
    ApiToken (..),
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Database.Beam (Beamable, Columnar, Identity, PrimaryKey, Table (..))
import GHC.Generics (Generic)
import Prelude hiding (id)

newtype ApiToken = ApiToken {unApiToken :: Text}
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (ToJSON, FromJSON)

data UserT f = User
  { id :: Columnar f Text,
    email :: Columnar f Text,
    passwordHash :: Columnar f Text,
    apiToken :: Columnar f Text
  }
  deriving (Generic)

type User = UserT Identity

deriving instance Show User

deriving instance Eq User

instance Beamable UserT

instance Table UserT where
  data PrimaryKey UserT f = UserIdPk (Columnar f Text) deriving (Generic)
  primaryKey (User i _ _ _) = UserIdPk i

deriving instance Show (PrimaryKey UserT Identity)

deriving instance Eq (PrimaryKey UserT Identity)

instance Beamable (PrimaryKey UserT)

data PublicUser = PublicUser
  { id :: Text,
    email :: Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

toPublicUser :: User -> PublicUser
toPublicUser (User u e _ _) = PublicUser u e
