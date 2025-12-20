{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}

module HIT.Types.Interval
  ( Interval (..),
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Deriving.Aeson (CamelToSnake, ConstructorTagModifier, CustomJSON (..))
import GHC.Generics (Generic)

data Interval = Daily | Weekly
  deriving (Show, Eq, Generic)
  deriving (ToJSON, FromJSON) via (CustomJSON '[ConstructorTagModifier CamelToSnake] Interval)
