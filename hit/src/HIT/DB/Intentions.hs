{-# LANGUAGE OverloadedStrings #-}
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

import Data.Text (Text)
import Data.Proxy (Proxy (..))
import Data.UUID qualified as UUID
import Database.Beam
import Database.Beam.Sqlite (runBeamSqlite)
import Database.SQLite.Simple (Connection)
import HIT.DB.Schema (IntentionTableSelector (..), hitDb)
import HIT.Types.Deadline (Deadline, DeadlineCodec)
import HIT.Types.Fraction (Fraction)
import HIT.Types.Intention (Intention, IntentionT (..))
import HIT.Types.Intention qualified as Intention (IntentionT (..))
import HIT.Types.User (PrimaryKey (UserId))
import HIT.Types.UUID ()

createIntention :: forall p. (IntentionTableSelector p, DeadlineCodec p) => Connection -> UUID.UUID -> Text -> Text -> Maybe Text -> Fraction -> Deadline p -> IO (Intention p)
createIntention conn intentionId uid iname idesc irate ideadline = do
  let i = Intention intentionId (UserId uid) iname idesc irate ideadline
  runBeamSqlite conn $
    runInsert $
      insert (intentionTable @p hitDb) $
        insertValues [i]
  pure i

getIntention :: forall p. (IntentionTableSelector p, DeadlineCodec p) => Connection -> Text -> UUID.UUID -> IO (Maybe (Intention p))
getIntention conn uid intentionId =
  runBeamSqlite conn $
    runSelectReturningOne $
      select $ do
        i <- all_ (intentionTable @p hitDb)
        guard_ (Intention.id i ==. val_ intentionId &&. Intention.user i ==. UserId (val_ uid))
        pure i

listIntentions :: forall p. (IntentionTableSelector p, DeadlineCodec p) => Connection -> Text -> IO [Intention p]
listIntentions conn uid =
  runBeamSqlite conn $
    runSelectReturningList $
      select $ do
        i <- all_ (intentionTable @p hitDb)
        guard_ (Intention.user i ==. UserId (val_ uid))
        pure i

updateIntention :: forall p. (IntentionTableSelector p, DeadlineCodec p) => Connection -> Text -> UUID.UUID -> Text -> Maybe Text -> Fraction -> Deadline p -> IO (Maybe (Intention p))
updateIntention conn uid intentionId iname idesc irate ideadline = do
  mExisting <- getIntention @p conn uid intentionId
  case mExisting of
    Nothing -> pure Nothing
    Just _ -> do
      runBeamSqlite conn $
        runUpdate $
          update
            (intentionTable @p hitDb)
            (\i ->
               mconcat
                 [ Intention.name i <-. val_ iname,
                   Intention.description i <-. val_ idesc,
                   Intention.rate i <-. val_ irate,
                   Intention.deadline i <-. val_ ideadline
                 ])
            (\i -> Intention.id i ==. val_ intentionId &&. Intention.user i ==. UserId (val_ uid))
      pure (Just (Intention intentionId (UserId uid) iname idesc irate ideadline))

deleteIntention :: forall p. (IntentionTableSelector p, DeadlineCodec p) => Proxy p -> Connection -> Text -> UUID.UUID -> IO Bool
deleteIntention _ conn uid intentionId = do
  mExisting <- getIntention @p conn uid intentionId
  case mExisting of
    Nothing -> pure False
    Just _ -> do
      runBeamSqlite conn $
        runDelete $
          delete (intentionTable @p hitDb) (\i -> Intention.id i ==. val_ intentionId &&. Intention.user i ==. UserId (val_ uid))
      pure True
