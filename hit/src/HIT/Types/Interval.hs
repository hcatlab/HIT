module HIT.Types.Interval
  ( Interval (..),
  )
where

import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Char (toLower)
import GHC.Generics (Generic)

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
