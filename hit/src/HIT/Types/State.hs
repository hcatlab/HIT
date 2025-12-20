module HIT.Types.State
  ( State (..),
  )
where

import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Char (toLower)
import GHC.Generics (Generic)

data State
  = Pending
  | InProgress
  | Done
  | Urgent
  deriving (Show, Eq, Generic)

instance ToJSON State where
  toJSON = toJSON . map toLower . show

instance FromJSON State where
  parseJSON v = do
    s <- parseJSON v
    case map toLower s of
      "pending" -> return Pending
      "inprogress" -> return InProgress
      "done" -> return Done
      "urgent" -> return Urgent
      _ -> fail "Invalid State"
