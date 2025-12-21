module HIT.Types.Fraction
  ( Fraction (..),
  )
where

import Data.Aeson (FromJSON (..), ToJSON (..), Value (String))
import Data.Text qualified as T
import GHC.Generics (Generic)

data Fraction = Fraction
  { numerator :: Int,
    denominator :: Int
  }
  deriving (Show, Eq, Generic)

instance ToJSON Fraction where
  toJSON (Fraction n d) = String $ T.pack (show n ++ "/" ++ show d)

instance FromJSON Fraction where
  parseJSON (String s) = do
    let str = T.unpack s
    let (numStr, denomStr) = break (== '/') str
    let n = read numStr :: Int
    case denomStr of
      '/' : rest -> do
        let d = read rest :: Int
        return $ Fraction n d
      _ -> fail "Invalid Fraction format"
  parseJSON _ = fail "Fraction must be a string"
