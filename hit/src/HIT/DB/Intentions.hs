{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedRecordDot #-}

module HIT.DB.Intentions
  ( createIntention,
    getIntention,
    listIntentions,
    updateIntention,
    deleteIntention,
  )
where

import Data.Maybe (listToMaybe)
import Database.Beam
import Database.Beam.Postgres (rUpper_, runBeamPostgres)
import Database.PostgreSQL.Simple (Connection, withTransaction)
import HIT.DB.Codec (decodeIntention, decodeOrThrow, encodeIntention)
import HIT.DB.Goals (setLinks, withLinks)
import HIT.DB.Rows (IntentionRow, IntentionRowT (..), PrimaryKey (UserRowId))
import HIT.DB.Rows qualified as Row (IntentionRowT (..))
import HIT.DB.Schema (HITDb (..), hitDb)
import HIT.Types.Identifier (IntentionIdentifier, Username)
import HIT.Types.Intention (Intention (..), Origin (..), SomeIntention (..))

createIntention :: Connection -> Intention origin interval -> IO ()
createIntention conn i =
  withTransaction conn $ do
    runBeamPostgres conn $ runInsert $ insert (intentions hitDb) $ insertValues [encodeIntention i]
    setLinks (intentionGoals hitDb) conn i.id i.goals

getIntention :: Connection -> Username -> IntentionIdentifier -> IO (Maybe SomeIntention)
getIntention conn username identifier = do
  rows <-
    runBeamPostgres conn $
      runSelectReturningList $
        select $ do
          i <- all_ (intentions hitDb)
          guard_ (Row.id i ==. val_ identifier &&. Row.user i ==. UserRowId (val_ username))
          pure i
  listToMaybe <$> decodeAll conn rows

-- | newest first
listIntentions :: Connection -> Username -> IO [SomeIntention]
listIntentions conn username = do
  rows <-
    runBeamPostgres conn $
      runSelectReturningList $
        select $
          orderBy_ (desc_ . rUpper_ . Row.window) $ do
            i <- all_ (intentions hitDb)
            guard_ (Row.user i ==. UserRowId (val_ username))
            pure i
  decodeAll conn rows

-- | decoded with their goal links, one query
decodeAll :: Connection -> [IntentionRow] -> IO [SomeIntention]
decodeAll conn rows = withLinks (intentionGoals hitDb) conn (.id) rows >>= traverse (decodeOrThrow . uncurry decodeIntention)

-- | caller checks ownership
updateIntention :: Connection -> Intention origin interval -> IO ()
updateIntention conn i =
  withTransaction conn $ do
    runBeamPostgres conn $ runUpdate $ save (intentions hitDb) (encodeIntention i)
    setLinks (intentionGoals hitDb) conn i.id i.goals

-- | only one-offs, habit occurrences belong to their habit
deleteIntention :: Connection -> Intention 'OneOff interval -> IO ()
deleteIntention conn i =
  runBeamPostgres conn $ runDelete $ delete (intentions hitDb) (\row -> Row.id row ==. val_ i.id)
