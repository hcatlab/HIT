{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module HIT.DB.Intentions
  ( createIntention,
    getIntention,
    getIntentionWithGoals,
    listIntentions,
    listIntentionsWithGoals,
    updateIntention,
    deleteIntention,
    listIntentionGoals,
  )
where

import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Data.UUID (UUID)
import Database.Beam
import Database.Beam.Postgres (runBeamPostgres)
import Database.PostgreSQL.Simple (Connection)
import HIT.DB.Schema (IntentionGoalT (..), IntentionTableSelector (..), goals, hitDb, intentionGoals)
import HIT.Types.Deadline (Deadline, DeadlineJson)
import HIT.Types.Fraction (Fraction)
import HIT.Types.Goal qualified as Goal
import HIT.Types.Intention (Intention, IntentionT (..))
import HIT.Types.Intention qualified as Intention (IntentionT (..))
import HIT.Types.Interval (IntervalTag (..))
import HIT.Types.User (PrimaryKey (UserId))

createIntention :: forall p. (IntentionTableSelector p, DeadlineJson p, IntervalTag p) => Connection -> UUID -> Text -> Text -> Maybe Text -> Fraction -> Deadline p -> NonEmpty UUID -> IO (Intention p)
createIntention conn intentionId uid iname idesc irate ideadline goalIds = do
  let i = Intention intentionId (UserId uid) iname idesc (intervalVal (Proxy @p)) irate ideadline
  runBeamPostgres conn $
    runInsert $
      insert (intentionTable @p hitDb) $
        insertValues [i]
  insertIntentionGoals conn intentionId goalIds
  pure i

getIntention :: forall p. (IntentionTableSelector p, DeadlineJson p, IntervalTag p) => Connection -> Text -> UUID -> IO (Maybe (Intention p))
getIntention conn uid intentionId =
  runBeamPostgres conn $
    runSelectReturningOne $
      select $ do
        i <- all_ (intentionTable @p hitDb)
        guard_ (Intention.id i ==. val_ intentionId &&. Intention.user i ==. UserId (val_ uid) &&. Intention.interval i ==. val_ (intervalVal (Proxy @p)))
        pure i

getIntentionWithGoals :: forall p. (IntentionTableSelector p, DeadlineJson p, IntervalTag p) => Connection -> Text -> UUID -> IO (Maybe (Intention p, [UUID]))
getIntentionWithGoals conn uid intentionId = do
  mIntention <- getIntention @p conn uid intentionId
  case mIntention of
    Nothing -> pure Nothing
    Just i -> do
      goalsList <- listIntentionGoals conn uid intentionId
      pure (Just (i, goalsList))

listIntentions :: forall p. (IntentionTableSelector p, DeadlineJson p, IntervalTag p) => Connection -> Text -> IO [Intention p]
listIntentions conn uid =
  runBeamPostgres conn $
    runSelectReturningList $
      select $ do
        i <- all_ (intentionTable @p hitDb)
        guard_ (Intention.user i ==. UserId (val_ uid) &&. Intention.interval i ==. val_ (intervalVal (Proxy @p)))
        pure i

listIntentionsWithGoals :: forall p. (IntentionTableSelector p, DeadlineJson p, IntervalTag p) => Connection -> Text -> IO [(Intention p, [UUID])]
listIntentionsWithGoals conn uid = do
  is <- listIntentions @p conn uid
  mapM (\i -> (i,) <$> listIntentionGoals conn uid (Intention.id i)) is

updateIntention :: forall p. (IntentionTableSelector p, DeadlineJson p, IntervalTag p) => Connection -> Text -> UUID -> Text -> Maybe Text -> Fraction -> Deadline p -> NonEmpty UUID -> IO (Maybe (Intention p))
updateIntention conn uid intentionId iname idesc irate ideadline goalIds = do
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
                        Intention.rate i <-. val_ irate,
                        Intention.deadline i <-. val_ ideadline
                      ]
                )
                (\i -> Intention.id i ==. val_ intentionId &&. Intention.user i ==. UserId (val_ uid) &&. Intention.interval i ==. val_ (intervalVal (Proxy @p)))
            )
        )
      replaceIntentionGoals conn intentionId goalIds
      pure (Just (Intention intentionId (UserId uid) iname idesc (intervalVal (Proxy @p)) irate ideadline))

deleteIntention :: forall p. (IntentionTableSelector p, DeadlineJson p, IntervalTag p) => Proxy p -> Connection -> Text -> UUID -> IO Bool
deleteIntention _ conn uid intentionId = do
  mExisting <- getIntention @p conn uid intentionId
  case mExisting of
    Nothing -> pure False
    Just _ -> do
      runBeamPostgres conn $
        runDelete $
          delete (intentionTable @p hitDb) (\i -> Intention.id i ==. val_ intentionId &&. Intention.user i ==. UserId (val_ uid) &&. Intention.interval i ==. val_ (intervalVal (Proxy @p)))
      pure True

-- Goal mapping helpers

insertIntentionGoals :: Connection -> UUID -> NonEmpty UUID -> IO ()
insertIntentionGoals conn iid goalIds =
  runBeamPostgres conn $
    runInsert $
      insert (intentionGoals hitDb) $
        insertValues (NE.toList (fmap (IntentionGoal iid) goalIds))

replaceIntentionGoals :: Connection -> UUID -> NonEmpty UUID -> IO ()
replaceIntentionGoals conn iid goalIds = do
  runBeamPostgres conn $
    runDelete $
      delete (intentionGoals hitDb) (\ig -> intentionId ig ==. val_ iid)
  insertIntentionGoals conn iid goalIds

listIntentionGoals :: Connection -> Text -> UUID -> IO [UUID]
listIntentionGoals conn uid iid =
  runBeamPostgres conn $
    runSelectReturningList $
      select $ do
        ig <- all_ (intentionGoals hitDb)
        g <- all_ (goals hitDb)
        guard_ (intentionId ig ==. val_ iid &&. goalId ig ==. Goal.id g &&. Goal.user g ==. UserId (val_ uid))
        pure (Goal.id g)
