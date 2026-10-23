module HIT.Validation
  ( parseGoalIds,
    validateGoalOwnership,
    requireNonEmpty,
  )
where

import Control.Monad (unless)
import Control.Monad.Except (MonadError)
import Data.ByteString.Lazy.Char8 qualified as LBS
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Text (Text)
import Data.Text qualified as T
import Data.UUID (UUID)
import Data.UUID qualified as UUID
import HIT.DB (ensureGoalsBelongToUser)
import HIT.Env (AppM, withDb)
import Servant

-- | 400 on bad uuid, deduped
parseGoalIds :: (MonadError ServerError m) => NonEmpty Text -> m (NonEmpty UUID)
parseGoalIds ids = case traverse UUID.fromText ids of
  Nothing -> throwError err400 {errBody = LBS.pack "Invalid goal id"}
  Just parsed -> pure (NE.nub parsed)

-- | 400 unless all goals belong to user
validateGoalOwnership :: Text -> NonEmpty UUID -> AppM ()
validateGoalOwnership uid goalIds = do
  ok <- withDb $ \conn -> ensureGoalsBelongToUser conn uid goalIds
  unless ok $
    throwError err400 {errBody = LBS.pack "goalIds must be non-empty and belong to the current user"}

-- | 400 on blank text
requireNonEmpty :: (MonadError ServerError m) => Text -> Text -> m Text
requireNonEmpty fieldName value
  | T.null (T.strip value) =
      throwError err400 {errBody = LBS.pack (T.unpack fieldName <> " must not be empty")}
  | otherwise = pure value
