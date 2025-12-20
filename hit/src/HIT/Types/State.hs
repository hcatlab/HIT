{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}

module HIT.Types.State
  ( State (..),
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Deriving.Aeson (CamelToSnake, ConstructorTagModifier, CustomJSON (..))
import GHC.Generics (Generic)

data State
  = Pending
  | InProgress
  | Done
  | Urgent
  deriving (Show, Eq, Generic)
  deriving (ToJSON, FromJSON) via (CustomJSON '[ConstructorTagModifier CamelToSnake] State)
