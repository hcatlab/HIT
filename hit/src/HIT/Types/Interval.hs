{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}

module HIT.Types.Interval
  ( Interval (..),
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Database.Beam (FromBackendRow, HasSqlEqualityCheck)
import Database.Beam.Backend.SQL (HasSqlValueSyntax)
import Database.Beam.Postgres (Postgres)
import Database.Beam.Postgres.Syntax (PgValueSyntax)
import Deriving.Aeson (CamelToSnake, ConstructorTagModifier, CustomJSON (..))
import GHC.Generics (Generic)
import HIT.DB.PgEnum (PgEnum (..))

-- | an interval is how often a habit recurs
data Interval
  = -- | one occurrence per matching day
    Daily
  | -- | one occurrence per iso week, monday to sunday
    Weekly
  deriving stock (Show, Eq, Ord, Enum, Bounded, Generic)
  deriving (ToJSON, FromJSON) via (CustomJSON '[ConstructorTagModifier CamelToSnake] Interval)
  deriving (HasSqlValueSyntax PgValueSyntax, FromBackendRow Postgres) via (PgEnum Interval)
  deriving anyclass (HasSqlEqualityCheck Postgres)
