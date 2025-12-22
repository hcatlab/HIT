{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}

module HIT.Types.Sort
  ( Sort (..),
  )
where

import Data.Aeson (FromJSON, Result (..), ToJSON, Value, fromJSON, toJSON)
import Database.Beam (FromBackendRow (..))
import Database.Beam.Backend.SQL (HasSqlValueSyntax (..))
import Database.Beam.Postgres (Postgres)
import Database.Beam.Postgres.Syntax (PgValueSyntax)
import Deriving.Aeson (CustomJSON (..), SumUntaggedValue)
import GHC.Generics (Generic)

data Sort = YesNo Bool | Times Int
  deriving (Show, Eq, Generic)
  deriving (ToJSON, FromJSON) via (CustomJSON '[SumUntaggedValue] Sort)

instance HasSqlValueSyntax PgValueSyntax Sort where
  sqlValueSyntax = sqlValueSyntax . toJSON

instance FromBackendRow Postgres Sort where
  fromBackendRow = do
    v <- fromBackendRow @Postgres @Value
    case fromJSON v of
      Success x -> pure x
      Error e -> fail e
