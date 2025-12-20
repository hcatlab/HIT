{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

module HIT.Types.Deadline
  ( Hours (..),
    Weekdays (..),
    Deadline (..),
  )
where

import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Kind (Type)
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
