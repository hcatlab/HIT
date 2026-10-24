{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module HIT.Types.User
  ( User (..),
    APIToken (..),
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Data.Time (UTCTime)
import Database.Beam (FromBackendRow, HasSqlEqualityCheck)
import Database.Beam.Backend.SQL (HasSqlValueSyntax)
import Database.Beam.Postgres (Postgres)
import Database.Beam.Postgres.Syntax (PgValueSyntax)
import Deriving.Aeson (CustomJSON (..), UnwrapUnaryRecords)
import GHC.Generics (Generic)
import HIT.Types.Identifier (Username)
import Prelude hiding (id)

-- | an api token is the bearer token of a user, json as a string
newtype APIToken = APIToken {unAPIToken :: Text}
  deriving stock (Show, Eq, Ord, Generic)
  deriving (ToJSON, FromJSON) via (CustomJSON '[UnwrapUnaryRecords] APIToken)
  deriving newtype (HasSqlValueSyntax PgValueSyntax, FromBackendRow Postgres)
  deriving anyclass (HasSqlEqualityCheck Postgres)

-- | a user account with credentials and settings
data User = User
  { -- | primary key
    id :: Username,
    -- | unique ignoring case
    email :: Text,
    -- | bcrypt hash of the password
    passwordHash :: Text,
    -- | bearer token, unique, replaced on each login
    apiToken :: APIToken,
    -- | iana zone name, the user's today is the current date there
    timezone :: Text,
    -- | creation time
    createdAt :: UTCTime,
    -- | last update, at least createdAt
    modifiedAt :: UTCTime
  }
  deriving stock (Show, Eq, Generic)
