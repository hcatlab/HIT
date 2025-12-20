{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}

module HIT.Types.Habit
  ( Habit (..),
  )
where

import Data.Aeson (FromJSON (..), ToJSON (..), defaultOptions, genericParseJSON)
import Data.Text (Text)
import Data.UUID (UUID)
import GHC.Generics (Generic)
import HIT.Types.Deadline (Deadline)
import HIT.Types.Fraction (Fraction)
import HIT.Types.Interval (Interval (Daily, Weekly))
import HIT.Types.Sort (Sort)

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
