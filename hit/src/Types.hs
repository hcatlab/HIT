{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Types where

import Data.Aeson (FromJSON (..), ToJSON (..))
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

data Interval = Daily | Weekly deriving (Show, Eq, Generic)

data Weekdays = Weekdays
  { monday :: [Int],
    tuesday :: [Int],
    wednesday :: [Int],
    thursday :: [Int],
    friday :: [Int],
    saturday :: [Int],
    sunday :: [Int]
  }
  deriving (Show, Eq, Generic)

data Deadline :: Interval -> Type where
  DayHours :: [Int] -> Deadline Daily
  WeekHours :: Weekdays -> Deadline Weekly

instance Show (Deadline p) where
  show (DayHours ts) = show ts
  show (WeekHours ws) = show ws

instance Eq (Deadline p) where
  (DayHours ts) == (DayHours ss) = ts == ss
  (WeekHours ws) == (WeekHours vs) = ws == vs

data Habit a p = Habit
  { id :: UUID,
    name :: Text,
    kind :: a,
    description :: Maybe Text,
    rate :: Fraction,
    deadline :: Deadline p
  }
  deriving (Show, Eq, Generic)

data Intention p = Intention 
  { id :: UUID,
    name :: Text,
    description :: Maybe Text,
    rate :: Fraction,
    deadline :: Deadline p
  }
  deriving (Show, Eq, Generic)

data State =
    Pending
  | InProgress
  | Done
  | Urgent
  deriving (Show, Eq, Generic)