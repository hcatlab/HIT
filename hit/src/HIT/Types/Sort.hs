module HIT.Types.Sort
  ( Sort (..),
  )
where

import Control.Applicative ((<|>))
import Data.Aeson (FromJSON (..), ToJSON (..))
import GHC.Generics (Generic)

data Sort = YesNo Bool | Times Int
  deriving (Show, Eq, Generic)

instance ToJSON Sort where
  toJSON (YesNo b) = toJSON b
  toJSON (Times n) = toJSON n

instance FromJSON Sort where
  parseJSON v = (YesNo <$> parseJSON v) <|> (Times <$> parseJSON v)
