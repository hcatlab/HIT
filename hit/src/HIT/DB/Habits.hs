{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module HIT.DB.Habits
  ( createHabit,
    getHabit,
    listHabits,
    updateHabit,
    deleteHabit,
  )
where

import Data.Text (Text)
import Data.Proxy (Proxy (..))
import Data.UUID qualified as UUID
import Database.Beam
import Database.Beam.Sqlite (runBeamSqlite)
import Database.SQLite.Simple (Connection)
import HIT.DB.Schema (HabitTableSelector (..), hitDb)
import HIT.Types.Deadline (Deadline, DeadlineCodec)
import HIT.Types.Fraction (Fraction)
import HIT.Types.Habit (Habit, HabitT (..))
import HIT.Types.Habit qualified as Habit (HabitT (..))
import HIT.Types.Sort (Sort)
import HIT.Types.User (PrimaryKey (UserId))

createHabit :: forall p. (HabitTableSelector p, DeadlineCodec p) => Connection -> UUID.UUID -> Text -> Text -> Maybe Text -> Sort -> Fraction -> Deadline p -> IO (Habit p)
createHabit conn habitId uid hname hdesc hsort hrate hdeadline = do
  let h = Habit habitId (UserId uid) hname hdesc hsort hrate hdeadline
  runBeamSqlite conn $
    runInsert $
      insert (habitTable @p hitDb) $
        insertValues [h]
  pure h

getHabit :: forall p. (HabitTableSelector p, DeadlineCodec p) => Connection -> Text -> UUID.UUID -> IO (Maybe (Habit p))
getHabit conn uid habitId =
  runBeamSqlite conn $
    runSelectReturningOne $
      select $ do
        h <- all_ (habitTable @p hitDb)
        guard_ (Habit.id h ==. val_ habitId &&. Habit.user h ==. UserId (val_ uid))
        pure h

listHabits :: forall p. (HabitTableSelector p, DeadlineCodec p) => Connection -> Text -> IO [Habit p]
listHabits conn uid =
  runBeamSqlite conn $
    runSelectReturningList $
      select $ do
        h <- all_ (habitTable @p hitDb)
        guard_ (Habit.user h ==. UserId (val_ uid))
        pure h

updateHabit :: forall p. (HabitTableSelector p, DeadlineCodec p) => Connection -> Text -> UUID.UUID -> Text -> Maybe Text -> Sort -> Fraction -> Deadline p -> IO (Maybe (Habit p))
updateHabit conn uid habitId hname hdesc hsort hrate hdeadline = do
  mExisting <- getHabit @p conn uid habitId
  case mExisting of
    Nothing -> pure Nothing
    Just _ -> do
      runBeamSqlite conn $
        runUpdate $
          update
            (habitTable @p hitDb)
            (\h ->
               mconcat
                 [ Habit.name h <-. val_ hname,
                   Habit.description h <-. val_ hdesc,
                   Habit.sort h <-. val_ hsort,
                   Habit.rate h <-. val_ hrate,
                   Habit.deadline h <-. val_ hdeadline
                 ])
            (\h -> Habit.id h ==. val_ habitId &&. Habit.user h ==. UserId (val_ uid))
      pure (Just (Habit habitId (UserId uid) hname hdesc hsort hrate hdeadline))

deleteHabit :: forall p. (HabitTableSelector p, DeadlineCodec p) => Proxy p -> Connection -> Text -> UUID.UUID -> IO Bool
deleteHabit _ conn uid habitId = do
  mExisting <- getHabit @p conn uid habitId
  case mExisting of
    Nothing -> pure False
    Just _ -> do
      runBeamSqlite conn $
        runDelete $
          delete (habitTable @p hitDb) (\h -> Habit.id h ==. val_ habitId &&. Habit.user h ==. UserId (val_ uid))
      pure True
