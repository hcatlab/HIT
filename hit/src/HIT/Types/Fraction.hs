module HIT.Types.Fraction
  ( Fraction (..),
  )
where

import Data.Aeson (FromJSON (..), ToJSON (..))
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
    case denomStr of
      '/' : rest -> do
        let d = read rest :: Int
        return $ Fraction n d
      _ -> fail "Invalid Fraction"
