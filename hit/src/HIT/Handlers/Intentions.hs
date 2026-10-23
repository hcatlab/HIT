{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module HIT.Handlers.Intentions
  ( intentionsServer,
    IntentionsApi,
  )
where

import Control.Monad.IO.Class (liftIO)
import Data.List.NonEmpty qualified as NE
import Data.UUID (UUID)
import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as UUIDv4
import Database.Beam (Identity)
import Database.PostgreSQL.Simple (Connection)
import HIT.Api.Intentions
  ( CreateIntentionRequest (..),
    IntentionDeadline (..),
    IntentionResponse (..),
    IntentionView (..),
    IntentionsApi,
    UpdateIntentionRequest (..),
  )
import HIT.Crud (CrudResource (..), crudServerTc)
import HIT.DB
  ( createIntention,
    deleteIntention,
    getIntentionWithGoals,
    listIntentionsWithGoals,
    updateIntention,
  )
import HIT.DB.Schema (IntentionTableSelector)
import HIT.Types.Deadline (DeadlineJson)
import HIT.Types.Intention qualified as Intention (IntentionT (..))
import HIT.Types.Interval (Interval (Daily, Weekly), IntervalTag)
import HIT.Types.User (User)
import HIT.Types.User qualified as User (UserT (..))
import HIT.Validation (parseGoalIds, requireNonEmpty, validateGoalOwnership)
import Servant
import Prelude hiding (id, read)

intentionsServer :: Connection -> User -> Server IntentionsApi
intentionsServer conn user =
  listIntentionsHandler conn user
    :<|> crudServerTc (IntentionsResource @'Daily conn) user
    :<|> crudServerTc (IntentionsResource @'Weekly conn) user

listIntentionsHandler :: Connection -> User -> Maybe Interval -> Handler [IntentionView]
listIntentionsHandler conn user mInterval = do
  let uid = User.id user
  case mInterval of
    Just Daily -> liftIO $ map (uncurry toIntentionViewDaily) <$> listIntentionsWithGoals @'Daily conn uid
    Just Weekly -> liftIO $ map (uncurry toIntentionViewWeekly) <$> listIntentionsWithGoals @'Weekly conn uid
    Nothing -> do
      daily <- liftIO $ map (uncurry toIntentionViewDaily) <$> listIntentionsWithGoals @'Daily conn uid
      weekly <- liftIO $ map (uncurry toIntentionViewWeekly) <$> listIntentionsWithGoals @'Weekly conn uid
      pure (daily <> weekly)

newtype IntentionsResource (p :: Interval) = IntentionsResource Connection

instance (IntentionTableSelector p, IntervalTag p, DeadlineJson p) => CrudResource (IntentionsResource p) where
  type Label (IntentionsResource p) = "intentionId"
  type InternalId (IntentionsResource p) = UUID
  type CreateReq (IntentionsResource p) = CreateIntentionRequest p
  type UpdateReq (IntentionsResource p) = UpdateIntentionRequest p
  type Resp (IntentionsResource p) = IntentionResponse p

  parseId _ = UUID.fromText

  list (IntentionsResource conn) u =
    liftIO $ map toIntentionResponseWithGoals <$> listIntentionsWithGoals @p conn (User.id u)

  create (IntentionsResource conn) u (CreateIntentionRequest iname idesc irate ideadline goalIdsText) = do
    _ <- requireNonEmpty "name" iname
    goalIds <- parseGoalIds goalIdsText
    validateGoalOwnership conn (User.id u) goalIds
    iid <- liftIO UUIDv4.nextRandom
    intention <- liftIO $ createIntention @p conn iid (User.id u) iname idesc irate ideadline goalIds
    pure (toIntentionResponseWithGoals (intention, NE.toList goalIds))

  read (IntentionsResource conn) u iid =
    liftIO $ fmap toIntentionResponseWithGoals <$> getIntentionWithGoals @p conn (User.id u) iid

  update (IntentionsResource conn) u iid (UpdateIntentionRequest iname idesc irate ideadline goalIdsText) = do
    _ <- requireNonEmpty "name" iname
    goalIds <- parseGoalIds goalIdsText
    validateGoalOwnership conn (User.id u) goalIds
    liftIO $ fmap (toIntentionResponseWithGoals . (,NE.toList goalIds)) <$> updateIntention @p conn (User.id u) iid iname idesc irate ideadline goalIds

  delete (IntentionsResource conn) u iid =
    liftIO $ deleteIntention @p Proxy conn (User.id u) iid

toIntentionViewDaily :: Intention.IntentionT 'Daily Identity -> [UUID] -> IntentionView
toIntentionViewDaily (Intention.Intention iid _ iname idesc _ irate ideadline createdAt modifiedAt) goalIds =
  IntentionView (UUID.toText iid) Daily iname idesc irate (DailyIntentionDeadline ideadline) (map UUID.toText goalIds) createdAt modifiedAt

toIntentionViewWeekly :: Intention.IntentionT 'Weekly Identity -> [UUID] -> IntentionView
toIntentionViewWeekly (Intention.Intention iid _ iname idesc _ irate ideadline createdAt modifiedAt) goalIds =
  IntentionView (UUID.toText iid) Weekly iname idesc irate (WeeklyIntentionDeadline ideadline) (map UUID.toText goalIds) createdAt modifiedAt

toIntentionResponseWithGoals :: (Intention.IntentionT p Identity, [UUID]) -> IntentionResponse p
toIntentionResponseWithGoals (Intention.Intention iid _ iname idesc _ irate ideadline createdAt modifiedAt, goalIds) =
  IntentionResponse (UUID.toText iid) iname idesc irate ideadline (map UUID.toText goalIds) createdAt modifiedAt
