{-# LANGUAGE DeriveAnyClass #-}

module HIT.Types.Goal
  ( Goal (..),
  )
where

import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Text (Text)
import Data.UUID (UUID)
import GHC.Generics (Generic)

data Goal = Goal
  { id :: UUID,
    name :: Text,
    description :: Maybe Text
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)
