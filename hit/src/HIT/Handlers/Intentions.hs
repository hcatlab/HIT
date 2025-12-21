{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module HIT.Handlers.Intentions
  ( intentionsServer,
    IntentionsApi,
  )
where

import Control.Monad.IO.Class (liftIO)
import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as UUIDv4
import Database.Beam (Identity)
import Database.Beam.Postgres (PgJSON (..))
import Database.PostgreSQL.Simple (Connection)
import HIT.Api.Intentions
  ( CreateIntentionRequest (..),
    IntentionApiFor,
    IntentionDeadline (..),
    IntentionResponse (..),
    IntentionView (..),
    IntentionsApi,
    UpdateIntentionRequest (..),
  )
import HIT.Crud (CrudResource (..), crudServerTc)
import HIT.DB (createIntention, deleteIntention, getIntention, listIntentions, updateIntention)
import HIT.DB.Schema (IntentionTableSelector)
import HIT.Types.Deadline (DeadlineCodec)
import HIT.Types.Intention qualified as Intention (IntentionT (..))
import HIT.Types.Interval (Interval (Daily, Weekly), IntervalTag)
import HIT.Types.User (User)
import HIT.Types.User qualified as User (UserT (..))
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
    Just Daily -> liftIO $ map toIntentionViewDaily <$> listIntentions @'Daily conn uid
    Just Weekly -> liftIO $ map toIntentionViewWeekly <$> listIntentions @'Weekly conn uid
    Nothing -> do
      daily <- liftIO $ map toIntentionViewDaily <$> listIntentions @'Daily conn uid
      weekly <- liftIO $ map toIntentionViewWeekly <$> listIntentions @'Weekly conn uid
      pure (daily <> weekly)

newtype IntentionsResource (p :: Interval) = IntentionsResource Connection

instance (IntentionTableSelector p, IntervalTag p, DeadlineCodec p) => CrudResource (IntentionsResource p) where
  type Label (IntentionsResource p) = "intentionId"
  type InternalId (IntentionsResource p) = UUID.UUID
  type CreateReq (IntentionsResource p) = CreateIntentionRequest p
  type UpdateReq (IntentionsResource p) = UpdateIntentionRequest p
  type Resp (IntentionsResource p) = IntentionResponse p

  parseId _ = UUID.fromText

  list (IntentionsResource conn) u =
    map toIntentionResponse <$> listIntentions @p conn (User.id u)

  create (IntentionsResource conn) u (CreateIntentionRequest iname idesc irate ideadline) = do
    iid <- liftIO UUIDv4.nextRandom
    toIntentionResponse <$> createIntention @p conn iid (User.id u) iname idesc irate ideadline

  read (IntentionsResource conn) u iid =
    fmap toIntentionResponse <$> getIntention @p conn (User.id u) iid

  update (IntentionsResource conn) u iid (UpdateIntentionRequest iname idesc irate ideadline) =
    fmap toIntentionResponse <$> updateIntention @p conn (User.id u) iid iname idesc irate ideadline

  delete (IntentionsResource conn) u iid =
    deleteIntention @p Proxy conn (User.id u) iid

toIntentionViewDaily :: Intention.IntentionT 'Daily Identity -> IntentionView
toIntentionViewDaily (Intention.Intention iid _ iname idesc _ (PgJSON irate) ideadline) =
  IntentionView (UUID.toText iid) Daily iname idesc irate (DailyIntentionDeadline ideadline)

toIntentionViewWeekly :: Intention.IntentionT 'Weekly Identity -> IntentionView
toIntentionViewWeekly (Intention.Intention iid _ iname idesc _ (PgJSON irate) ideadline) =
  IntentionView (UUID.toText iid) Weekly iname idesc irate (WeeklyIntentionDeadline ideadline)

toIntentionResponse :: Intention.IntentionT p Identity -> IntentionResponse p
toIntentionResponse (Intention.Intention iid _ iname idesc _ (PgJSON irate) ideadline) =
  IntentionResponse (UUID.toText iid) iname idesc irate ideadline
