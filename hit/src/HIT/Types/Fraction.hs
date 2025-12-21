module HIT.Types.Fraction
  ( Fraction (..),
  )
where

import Data.Aeson (FromJSON (..), ToJSON (..), Value (String), eitherDecode, encode)
import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T
import Data.Text.Encoding qualified as Text
import Database.Beam (FromBackendRow (..))
import Database.Beam.Backend.SQL (HasSqlValueSyntax (..))
import Database.Beam.Sqlite (Sqlite)
import Database.Beam.Sqlite.Syntax (SqliteValueSyntax)
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

-- Store Fraction as JSON-encoded TEXT in SQLite
instance HasSqlValueSyntax SqliteValueSyntax Fraction where
  sqlValueSyntax = sqlValueSyntax . Text.decodeUtf8 . LBS.toStrict . encode

instance FromBackendRow Sqlite Fraction where
  fromBackendRow = do
    t <- fromBackendRow @Sqlite @T.Text
    case eitherDecode (LBS.fromStrict (Text.encodeUtf8 t)) of
      Right f -> pure f
      Left _ -> fail "Invalid fraction"
