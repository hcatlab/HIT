{-# LANGUAGE OverloadedRecordDot #-}

module HIT.DB.Habits
  ( createHabit,
    getHabit,
    listHabits,
    updateHabit,
    deleteHabit,
  )
where

import Data.Maybe (listToMaybe)
import Database.Beam
import Database.Beam.Backend.SQL.BeamExtensions (runDeleteReturningList)
import Database.Beam.Postgres (runBeamPostgres)
import Database.PostgreSQL.Simple (Connection, withTransaction)
import HIT.DB.Codec (decodeHabit, decodeOrThrow, encodeHabit)
import HIT.DB.Goals (setLinks, withLinks)
import HIT.DB.Rows
  ( HabitRow,
    HabitRowT (..),
    PrimaryKey (UserRowId),
  )
import HIT.DB.Rows qualified as Row (HabitRowT (..))
import HIT.DB.Schema (HITDb (..), hitDb)
import HIT.Types.Habit (Habit (..), SomeHabit)
import HIT.Types.Identifier (HabitIdentifier, Username)

createHabit :: Connection -> Habit interval -> IO ()
createHabit conn h =
  withTransaction conn $ do
    runBeamPostgres conn $ runInsert $ insert (habits hitDb) $ insertValues [encodeHabit h]
    setLinks (habitGoals hitDb) conn h.id h.goals

getHabit :: Connection -> Username -> HabitIdentifier -> IO (Maybe SomeHabit)
getHabit conn username identifier = do
  rows <-
    runBeamPostgres conn $
      runSelectReturningList $
        select $ do
          h <- all_ (habits hitDb)
          guard_ (Row.id h ==. val_ identifier &&. Row.user h ==. UserRowId (val_ username))
          pure h
  listToMaybe <$> decodeAll conn rows

listHabits :: Connection -> Username -> IO [SomeHabit]
listHabits conn username = do
  rows <-
    runBeamPostgres conn $
      runSelectReturningList $
        select $
          orderBy_ (asc_ . Row.createdAt) $ do
            h <- all_ (habits hitDb)
            guard_ (Row.user h ==. UserRowId (val_ username))
            pure h
  decodeAll conn rows

-- | decoded with their goal links, one query
decodeAll :: Connection -> [HabitRow] -> IO [SomeHabit]
decodeAll conn rows = withLinks (habitGoals hitDb) conn (.id) rows >>= traverse (decodeOrThrow . uncurry decodeHabit)

-- | caller checks ownership
updateHabit :: Connection -> Habit interval -> IO ()
updateHabit conn h =
  withTransaction conn $ do
    runBeamPostgres conn $ runUpdate $ save (habits hitDb) (encodeHabit h)
    setLinks (habitGoals hitDb) conn h.id h.goals

deleteHabit :: Connection -> Username -> HabitIdentifier -> IO Bool
deleteHabit conn username identifier =
  fmap (not . null)
    $ runBeamPostgres conn
    $ runDeleteReturningList
    $ delete (habits hitDb) (\h -> Row.id h ==. val_ identifier &&. Row.user h ==. UserRowId (val_ username))
