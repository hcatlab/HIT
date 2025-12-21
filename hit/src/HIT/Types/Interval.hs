{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module HIT.Types.Interval
  ( Interval (..),
    IntervalTag (..),
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Data.Text qualified as T
import Database.Beam (FromBackendRow (..), HasSqlEqualityCheck)
import Database.Beam.Backend.SQL (HasSqlValueSyntax (..))
import Database.Beam.Postgres (Postgres)
import Database.Beam.Postgres.Syntax (PgValueSyntax)
import Deriving.Aeson (CamelToSnake, ConstructorTagModifier, CustomJSON (..))
import GHC.Generics (Generic)
import Servant.API (FromHttpApiData (..), ToHttpApiData (..))

data Interval = Daily | Weekly
  deriving (Show, Eq, Generic)
  deriving (ToJSON, FromJSON) via (CustomJSON '[ConstructorTagModifier CamelToSnake] Interval)

class IntervalTag (p :: Interval) where
  intervalVal :: Proxy p -> Interval

instance IntervalTag 'Daily where
  intervalVal _ = Daily

instance IntervalTag 'Weekly where
  intervalVal _ = Weekly

encodeInterval :: Interval -> Text
encodeInterval Daily = "daily"
encodeInterval Weekly = "weekly"

decodeInterval :: Text -> Maybe Interval
decodeInterval "daily" = Just Daily
decodeInterval "weekly" = Just Weekly
decodeInterval _ = Nothing

instance HasSqlValueSyntax PgValueSyntax Interval where
  sqlValueSyntax = sqlValueSyntax . encodeInterval

instance FromBackendRow Postgres Interval where
  fromBackendRow = do
    t <- fromBackendRow @Postgres @Text
    maybe (fail "Invalid interval") pure (decodeInterval t)

instance HasSqlEqualityCheck Postgres Interval

instance FromHttpApiData Interval where
  parseQueryParam raw =
    case T.toLower raw of
      "daily" -> Right Daily
      "weekly" -> Right Weekly
      _ -> Left "interval must be daily or weekly"

instance ToHttpApiData Interval where
  toUrlPiece = encodeInterval
