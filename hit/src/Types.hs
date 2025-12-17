{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

module Types where

import Control.Applicative ((<|>))
import Data.Aeson (FromJSON (..), ToJSON (..), defaultOptions, genericParseJSON)
import Data.Char (toLower)
import Data.Kind
import Data.Text (Text)
import Data.UUID (UUID)
import GHC.Generics (Generic)

data Fraction = Fraction
  { numerator :: Int,
    denominator :: Int
  }
  deriving (Show, Eq, Generic)

instance ToJSON Fraction where
  toJSON (Fraction n d) = toJSON (show n ++ "/" ++ show d)

instance FromJSON Fraction where
  parseJSON v = do
    s <- parseJSON v
    let (numStr, denomStr) = break (== '/') s
    let n = read numStr :: Int
    let d = read (tail denomStr) :: Int
    return $ Fraction n d

data Interval = Daily | Weekly
  deriving (Show, Eq, Generic)

instance ToJSON Interval where
  toJSON = toJSON . map toLower . show

instance FromJSON Interval where
  parseJSON v = do
    s <- parseJSON v
    case map toLower s of
      "daily" -> return Daily
      "weekly" -> return Weekly
      _ -> fail "Invalid Interval"

data Weekdays = Weekdays
  { monday :: [Int],
    tuesday :: [Int],
    wednesday :: [Int],
    thursday :: [Int],
    friday :: [Int],
    saturday :: [Int],
    sunday :: [Int]
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data Deadline :: Interval -> Type where
  DayHours :: [Int] -> Deadline Daily
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

data Sort = YesNo Bool | Times Int
  deriving (Show, Eq, Generic)

instance ToJSON Sort where
  toJSON (YesNo b) = toJSON b
  toJSON (Times n) = toJSON n

instance FromJSON Sort where
  parseJSON v = (YesNo <$> parseJSON v) <|> (Times <$> parseJSON v)

data Habit p = Habit
  { id :: UUID,
    name :: Text,
    sort :: Sort,
    description :: Maybe Text,
    rate :: Fraction,
    deadline :: Deadline p
  }
  deriving (Show, Eq, Generic, ToJSON)

instance FromJSON (Habit Daily) where
  parseJSON = genericParseJSON defaultOptions

instance FromJSON (Habit Weekly) where
  parseJSON = genericParseJSON defaultOptions

data Intention p = Intention
  { id :: UUID,
    name :: Text,
    description :: Maybe Text,
    rate :: Fraction,
    deadline :: Deadline p
  }
  deriving (Show, Eq, Generic, ToJSON)

instance FromJSON (Intention Daily) where
  parseJSON = genericParseJSON defaultOptions

instance FromJSON (Intention Weekly) where
  parseJSON = genericParseJSON defaultOptions

data State
  = Pending
  | InProgress
  | Done
  | Urgent
  deriving (Show, Eq, Generic)

instance ToJSON State where
  toJSON = toJSON . map toLower . show

instance FromJSON State where
  parseJSON v = do
    s <- parseJSON v
    case map toLower s of
      "pending" -> return Pending
      "inprogress" -> return InProgress
      "done" -> return Done
      "urgent" -> return Urgent
      _ -> fail "Invalid State"