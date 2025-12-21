{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

module HIT.Types.Deadline
  ( Hours (..),
    Weekdays (..),
    Deadline (..),
    DeadlineCodec (..),
  )
where

import Data.Aeson (FromJSON (..), ToJSON (..), eitherDecode, encode)
import Data.ByteString.Lazy qualified as LBS
import Data.Kind (Type)
import Data.Text (Text)
import Data.Text.Encoding qualified as Text
import Database.Beam (FromBackendRow (..))
import Database.Beam.Backend.SQL (HasSqlValueSyntax (..))
import Database.Beam.Postgres (Postgres)
import Database.Beam.Postgres.Syntax (PgValueSyntax)
import Deriving.Aeson (CustomJSON (..), UnwrapUnaryRecords)
import GHC.Generics (Generic)
import HIT.Types.Interval (Interval (Daily, Weekly))

newtype Hours = Hours {getHours :: [Int]}
  deriving stock (Show, Eq, Generic)
  deriving (ToJSON, FromJSON) via (CustomJSON '[UnwrapUnaryRecords] Hours)

data Weekdays = Weekdays
  { monday :: Hours,
    tuesday :: Hours,
    wednesday :: Hours,
    thursday :: Hours,
    friday :: Hours,
    saturday :: Hours,
    sunday :: Hours
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data Deadline :: Interval -> Type where
  DayHours :: Hours -> Deadline Daily
  WeekHours :: Weekdays -> Deadline Weekly

deriving instance Show (Deadline p)

deriving instance Eq (Deadline p)

instance ToJSON (Deadline p) where
  toJSON (DayHours hrs) = toJSON hrs
  toJSON (WeekHours wds) = toJSON wds

instance FromJSON (Deadline Daily) where
  parseJSON v = DayHours <$> parseJSON v

instance FromJSON (Deadline Weekly) where
  parseJSON v = WeekHours <$> parseJSON v

class DeadlineCodec (p :: Interval) where
  encodeDeadline :: Deadline p -> Text
  decodeDeadline :: Text -> Maybe (Deadline p)

instance DeadlineCodec 'Daily where
  encodeDeadline = Text.decodeUtf8 . LBS.toStrict . encode
  decodeDeadline t = do
    case eitherDecode (LBS.fromStrict (Text.encodeUtf8 t)) of
      Right (hrs :: Hours) -> Just (DayHours hrs)
      Left _ -> Nothing

instance DeadlineCodec 'Weekly where
  encodeDeadline = Text.decodeUtf8 . LBS.toStrict . encode
  decodeDeadline t =
    case eitherDecode (LBS.fromStrict (Text.encodeUtf8 t)) of
      Right (wds :: Weekdays) -> Just (WeekHours wds)
      Left _ -> Nothing

-- Store Deadline as JSONB in PostgreSQL for better querying
instance (DeadlineCodec p) => HasSqlValueSyntax PgValueSyntax (Deadline p) where
  sqlValueSyntax = sqlValueSyntax . encodeDeadline

instance (DeadlineCodec p) => FromBackendRow Postgres (Deadline p) where
  fromBackendRow = do
    t <- fromBackendRow @Postgres @Text
    maybe (fail "Invalid deadline") pure (decodeDeadline t)
