{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}

module HIT.Types.Intention
  ( Intention (..),
  )
where

import Data.Aeson (FromJSON (..), ToJSON (..), defaultOptions, genericParseJSON)
import Data.Text (Text)
import Data.UUID (UUID)
import GHC.Generics (Generic)
import HIT.Types.Deadline (Deadline)
import HIT.Types.Fraction (Fraction)
import HIT.Types.Interval (Interval (Daily, Weekly))

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
