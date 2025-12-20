{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}

module HIT.Types.Sort
  ( Sort (..),
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Deriving.Aeson (CustomJSON (..), SumUntaggedValue)
import GHC.Generics (Generic)

data Sort = YesNo Bool | Times Int
  deriving (Show, Eq, Generic)
  deriving (ToJSON, FromJSON) via (CustomJSON '[SumUntaggedValue] Sort)
