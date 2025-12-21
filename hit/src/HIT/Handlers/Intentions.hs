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

import Data.Proxy (Proxy (..))
import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as UUIDv4
import Database.Beam (Identity)
import Database.Beam.Postgres (PgJSON (..))
import Database.PostgreSQL.Simple (Connection)
import HIT.Api.Intentions (CreateIntentionRequest (..), IntentionApiFor, IntentionResponse (..), IntentionsApi, UpdateIntentionRequest (..))
import HIT.Crud (CrudApiFor, CrudResource (..), crudServerTc)
import HIT.DB (createIntention, deleteIntention, getIntention, listIntentions, updateIntention)
import HIT.DB.Schema (IntentionTableSelector)
import HIT.Types.Deadline (DeadlineCodec)
import HIT.Types.Intention qualified as Intention (IntentionT (..))
import HIT.Types.Interval (Interval (Daily, Weekly))
import HIT.Types.User (User)
import HIT.Types.User qualified as User (UserT (..))
import Servant
import Prelude hiding (id, read)

newtype IntentionsResource (p :: Interval) = IntentionsResource Connection

instance (IntentionTableSelector p, DeadlineCodec p) => CrudResource (IntentionsResource p) where
  type Label (IntentionsResource p) = "intentionId"
  type InternalId (IntentionsResource p) = UUID.UUID
  type CreateReq (IntentionsResource p) = CreateIntentionRequest p
  type UpdateReq (IntentionsResource p) = UpdateIntentionRequest p
  type Resp (IntentionsResource p) = IntentionResponse p

  parseId _ = UUID.fromText

  list (IntentionsResource conn) u =
    map toIntentionResponse <$> listIntentions @p conn (User.id u)

  create (IntentionsResource conn) u (CreateIntentionRequest iname idesc irate ideadline) = do
    iid <- UUIDv4.nextRandom
    toIntentionResponse <$> createIntention @p conn iid (User.id u) iname idesc irate ideadline

  read (IntentionsResource conn) u iid =
    fmap toIntentionResponse <$> getIntention @p conn (User.id u) iid

  update (IntentionsResource conn) u iid (UpdateIntentionRequest iname idesc irate ideadline) =
    fmap toIntentionResponse <$> updateIntention @p conn (User.id u) iid iname idesc irate ideadline

  delete (IntentionsResource conn) u iid =
    deleteIntention @p Proxy conn (User.id u) iid

intentionsServer :: Connection -> User -> Server IntentionsApi
intentionsServer conn user =
  crudServerTc (IntentionsResource @'Daily conn) user
    :<|> crudServerTc (IntentionsResource @'Weekly conn) user

toIntentionResponse :: Intention.IntentionT p Identity -> IntentionResponse p
toIntentionResponse (Intention.Intention iid _ iname idesc (PgJSON irate) ideadline) =
  IntentionResponse (UUID.toText iid) iname idesc irate ideadline
