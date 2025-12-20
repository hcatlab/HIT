{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
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
import Deriving.Aeson (CustomJSON (..), UnwrapUnaryRecords)
import GHC.Generics (Generic)
import Prelude hiding (id)

newtype ApiToken = ApiToken {unApiToken :: Text}
  deriving (Show, Eq, Ord, Generic)
  deriving (ToJSON, FromJSON) via (CustomJSON '[UnwrapUnaryRecords] ApiToken)

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
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

toPublicUser :: User -> PublicUser
toPublicUser (User u e _ _) = PublicUser u e
