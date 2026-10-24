{-# LANGUAGE ScopedTypeVariables #-}

module HIT.DB.PgEnum
  ( PgEnum (..),
  )
where

import Data.Aeson (FromJSON, Result (..), ToJSON, Value (String), fromJSON, toJSON)
import Data.Text.Encoding (decodeUtf8')
import Data.Typeable (Typeable)
import Database.Beam (FromBackendRow)
import Database.Beam.Backend.SQL (HasSqlValueSyntax (..))
import Database.Beam.Postgres (Postgres)
import Database.Beam.Postgres.Syntax (PgValueSyntax)
import Database.PostgreSQL.Simple.FromField (FromField (..), ResultError (..), returnError)

-- | a pg enum is a postgres enum column labelled by the json string of its value
newtype PgEnum a = PgEnum a

instance (ToJSON a) => HasSqlValueSyntax PgValueSyntax (PgEnum a) where
  sqlValueSyntax (PgEnum a) = case toJSON a of
    String t -> sqlValueSyntax t
    other -> error ("PgEnum: not a json string: " <> show other)

-- | any text-like column
instance (FromJSON a, Typeable a) => FromField (PgEnum a) where
  fromField field bytes = case bytes of
    Nothing -> returnError UnexpectedNull field ""
    Just raw -> case decodeUtf8' raw of
      Left e -> returnError ConversionFailed field (show e)
      Right t -> case fromJSON (String t) of
        Success a -> pure (PgEnum a)
        Error e -> returnError ConversionFailed field e

instance (FromJSON a, Typeable a) => FromBackendRow Postgres (PgEnum a)
