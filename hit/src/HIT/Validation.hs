module HIT.Validation
  ( validateGoalOwnership,
    validateTimezone,
    orBadRequest,
  )
where

import Control.Monad (unless)
import Control.Monad.Except (MonadError)
import Data.ByteString.Lazy.Char8 qualified as LBS
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Text (Text)
import HIT.DB (ensureGoalsBelongToUser, validTimezone)
import HIT.Env (AppM, withDb)
import HIT.Types.Identifier (GoalIdentifier, Username)
import Servant

-- | the goals without duplicates, 400 unless all belong to the user
validateGoalOwnership :: Username -> NonEmpty GoalIdentifier -> AppM (NonEmpty GoalIdentifier)
validateGoalOwnership username goalIds = do
  let unique = NE.nub goalIds
  ok <- withDb $ \conn -> ensureGoalsBelongToUser conn username unique
  unless ok $
    throwError err400 {errBody = LBS.pack "goalIds must belong to the current user"}
  pure unique

-- | 400 unless an iana timezone known to postgres
validateTimezone :: Text -> AppM Text
validateTimezone tz = do
  ok <- withDb $ \conn -> validTimezone conn tz
  orBadRequest (if ok then Right tz else Left "unknown timezone")

-- | the value, or a 400 with the message
orBadRequest :: (MonadError ServerError m) => Either String a -> m a
orBadRequest = either (\message -> throwError err400 {errBody = LBS.pack message}) pure
