{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module HIT.DB.Intentions
  ( createIntention,
    getIntention,
    listIntentions,
    updateIntention,
    deleteIntention,
  )
where

import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Data.UUID qualified as UUID
import Database.Beam
import Database.Beam.Postgres (PgJSON (..), runBeamPostgres)
import Database.PostgreSQL.Simple (Connection)
import HIT.DB.Schema (IntentionTableSelector (..), hitDb)
import HIT.Types.Deadline (Deadline, DeadlineCodec)
import HIT.Types.Fraction (Fraction)
import HIT.Types.Intention (Intention, IntentionT (..))
import HIT.Types.Intention qualified as Intention (IntentionT (..))
import HIT.Types.Interval (IntervalTag (..))
import HIT.Types.User (PrimaryKey (UserId))

createIntention :: forall p. (IntentionTableSelector p, DeadlineCodec p, IntervalTag p) => Connection -> UUID.UUID -> Text -> Text -> Maybe Text -> Fraction -> Deadline p -> IO (Intention p)
createIntention conn intentionId uid iname idesc irate ideadline = do
  let i = Intention intentionId (UserId uid) iname idesc (intervalVal (Proxy @p)) (PgJSON irate) ideadline
  runBeamPostgres conn $
    runInsert $
      insert (intentionTable @p hitDb) $
        insertValues [i]
  pure i

getIntention :: forall p. (IntentionTableSelector p, DeadlineCodec p, IntervalTag p) => Connection -> Text -> UUID.UUID -> IO (Maybe (Intention p))
getIntention conn uid intentionId =
  runBeamPostgres conn $
    runSelectReturningOne $
      select $ do
        i <- all_ (intentionTable @p hitDb)
        guard_ (Intention.id i ==. val_ intentionId &&. Intention.user i ==. UserId (val_ uid) &&. Intention.interval i ==. val_ (intervalVal (Proxy @p)))
        pure i

listIntentions :: forall p. (IntentionTableSelector p, DeadlineCodec p, IntervalTag p) => Connection -> Text -> IO [Intention p]
listIntentions conn uid =
  runBeamPostgres conn $
    runSelectReturningList $
      select $ do
        i <- all_ (intentionTable @p hitDb)
        guard_ (Intention.user i ==. UserId (val_ uid) &&. Intention.interval i ==. val_ (intervalVal (Proxy @p)))
        pure i

updateIntention :: forall p. (IntentionTableSelector p, DeadlineCodec p, IntervalTag p) => Connection -> Text -> UUID.UUID -> Text -> Maybe Text -> Fraction -> Deadline p -> IO (Maybe (Intention p))
updateIntention conn uid intentionId iname idesc irate ideadline = do
  mExisting <- getIntention @p conn uid intentionId
  case mExisting of
    Nothing -> pure Nothing
    Just _ -> do
      runBeamPostgres
        conn
        ( runUpdate
            ( update
                (intentionTable @p hitDb)
                ( \i ->
                    mconcat
                      [ Intention.name i <-. val_ iname,
                        Intention.description i <-. val_ idesc,
                        Intention.interval i <-. val_ (intervalVal (Proxy @p)),
                        Intention.rate i <-. val_ (PgJSON irate),
                        Intention.deadline i <-. val_ ideadline
                      ]
                )
                (\i -> Intention.id i ==. val_ intentionId &&. Intention.user i ==. UserId (val_ uid) &&. Intention.interval i ==. val_ (intervalVal (Proxy @p)))
            )
        )
      pure (Just (Intention intentionId (UserId uid) iname idesc (intervalVal (Proxy @p)) (PgJSON irate) ideadline))

deleteIntention :: forall p. (IntentionTableSelector p, DeadlineCodec p, IntervalTag p) => Proxy p -> Connection -> Text -> UUID.UUID -> IO Bool
deleteIntention _ conn uid intentionId = do
  mExisting <- getIntention @p conn uid intentionId
  case mExisting of
    Nothing -> pure False
    Just _ -> do
      runBeamPostgres conn $
        runDelete $
          delete (intentionTable @p hitDb) (\i -> Intention.id i ==. val_ intentionId &&. Intention.user i ==. UserId (val_ uid) &&. Intention.interval i ==. val_ (intervalVal (Proxy @p)))
      pure True
