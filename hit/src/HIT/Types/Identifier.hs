{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module HIT.Types.Identifier
  ( Entity (..),
    Identifier (..),
    GoalIdentifier,
    HabitIdentifier,
    IntentionIdentifier,
    Username (..),
    EntityKey,
    newIdentifier,
    newIdentifiers,
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.Kind (Type)
import Data.Text (Text)
import Data.UUID (UUID)
import Database.Beam (FromBackendRow, HasSqlEqualityCheck)
import Database.Beam.Backend.SQL (HasSqlValueSyntax)
import Database.Beam.Postgres (Postgres)
import Database.Beam.Postgres.Syntax (PgValueSyntax)
import Database.PostgreSQL.Simple (Connection, Only (..), query)
import Servant (FromHttpApiData, ToHttpApiData)

-- | an entity is a kind of stored record, indexing identifiers and routes
data Entity
  = -- | an account
    UserEntity
  | -- | a long-term goal
    GoalEntity
  | -- | a recurring intention
    HabitEntity
  | -- | a one-off or generated intention
    IntentionEntity

-- | an identifier is the uuid of one entity, json and urls as the uuid string
type Identifier :: Entity -> Type
newtype Identifier entity = Identifier
  { -- | the underlying uuid
    uuid :: UUID
  }
  deriving stock (Show)
  deriving newtype (Eq, Ord, ToJSON, FromJSON, FromHttpApiData, ToHttpApiData, HasSqlValueSyntax PgValueSyntax, FromBackendRow Postgres)
  deriving anyclass (HasSqlEqualityCheck Postgres)

-- | a goal identifier
type GoalIdentifier = Identifier 'GoalEntity

-- | a habit identifier
type HabitIdentifier = Identifier 'HabitEntity

-- | an intention identifier
type IntentionIdentifier = Identifier 'IntentionEntity

-- | a username is identifies a user
newtype Username = Username
  { -- | the name
    name :: Text
  }
  deriving stock (Show)
  deriving newtype (Eq, Ord, ToJSON, FromJSON, FromHttpApiData, ToHttpApiData, HasSqlValueSyntax PgValueSyntax, FromBackendRow Postgres)
  deriving anyclass (HasSqlEqualityCheck Postgres)

-- | an entity key is what an entity is addressed by
type EntityKey :: Entity -> Type
type family EntityKey entity where
  EntityKey 'UserEntity = Username
  EntityKey entity = Identifier entity

-- | a fresh time-ordered identifier, postgres uuidv7
newIdentifier :: Connection -> IO (Identifier entity)
newIdentifier conn = do
  identifiers <- newIdentifiers conn 1
  case identifiers of
    [identifier] -> pure identifier
    _ -> fail "newIdentifier: no row"

-- | n fresh time-ordered identifiers, one query
newIdentifiers :: Connection -> Int -> IO [Identifier entity]
newIdentifiers conn n = map (Identifier . fromOnly) <$> query conn "SELECT uuidv7() FROM generate_series(1, ?)" (Only n)
