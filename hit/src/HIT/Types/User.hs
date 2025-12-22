{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module HIT.Types.User
  ( UserT (..),
    User,
    PublicUser (..),
    toPublicUser,
    ApiToken (..),
    PrimaryKey (UserId),
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Data.Time (UTCTime)
import Database.Beam (Beamable, Columnar, Identity, PrimaryKey, Table (..))
import Deriving.Aeson (CustomJSON (..), UnwrapUnaryRecords)
import GHC.Generics (Generic)
import Prelude hiding (id)

newtype ApiToken = ApiToken {unApiToken :: Text}
  deriving stock (Show, Eq, Ord, Generic)
  deriving (ToJSON, FromJSON) via (CustomJSON '[UnwrapUnaryRecords] ApiToken)

data UserT f = User
  { id :: Columnar f Text,
    email :: Columnar f Text,
    passwordHash :: Columnar f Text,
    apiToken :: Columnar f Text,
    createdAt :: Columnar f UTCTime,
    modifiedAt :: Columnar f UTCTime
  }
  deriving (Generic)

type User = UserT Identity

deriving instance Show User

deriving instance Eq User

instance Beamable UserT

instance Table UserT where
  data PrimaryKey UserT f = UserId (Columnar f Text) deriving (Generic)
  primaryKey (User i _ _ _ _ _) = UserId i

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
toPublicUser (User u e _ _ _ _) = PublicUser u e
