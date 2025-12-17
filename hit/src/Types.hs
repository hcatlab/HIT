module Types where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Data.UUID (UUID)
import GHC.Generics (Generic)

data Fraction = Fraction
  { numerator :: Int,
    denominator :: Int
  }
  deriving (Show, Eq, Generic)

data Habit a = Habit
  { id :: UUID,
    name :: Text,
    description :: Maybe Text,
    rate :: Fraction
  }
  deriving (Show, Eq, Generic)