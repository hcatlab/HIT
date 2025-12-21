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

import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Data.UUID qualified as UUID
import Database.Beam
import Database.Beam.Postgres (PgJSON (..), runBeamPostgres)
import Database.PostgreSQL.Simple (Connection)
import HIT.DB.Schema (HabitTableSelector (..), hitDb)
import HIT.Types.Deadline (Deadline, DeadlineCodec)
import HIT.Types.Fraction (Fraction)
import HIT.Types.Habit (Habit, HabitT (..))
import HIT.Types.Habit qualified as Habit (HabitT (..))
import HIT.Types.Interval (IntervalTag (..))
import HIT.Types.Sort (Sort)
import HIT.Types.User (PrimaryKey (UserId))

createHabit :: forall p. (HabitTableSelector p, DeadlineCodec p, IntervalTag p) => Connection -> UUID.UUID -> Text -> Text -> Maybe Text -> Sort -> Fraction -> Deadline p -> IO (Habit p)
createHabit conn habitId uid hname hdesc hsort hrate hdeadline = do
  let h = Habit habitId (UserId uid) hname hdesc (intervalVal (Proxy @p)) (PgJSON hsort) (PgJSON hrate) hdeadline
  runBeamPostgres conn $
    runInsert $
      insert (habitTable @p hitDb) $
        insertValues [h]
  pure h

getHabit :: forall p. (HabitTableSelector p, DeadlineCodec p, IntervalTag p) => Connection -> Text -> UUID.UUID -> IO (Maybe (Habit p))
getHabit conn uid habitId =
  runBeamPostgres conn $
    runSelectReturningOne $
      select $ do
        h <- all_ (habitTable @p hitDb)
        guard_ (Habit.id h ==. val_ habitId &&. Habit.user h ==. UserId (val_ uid) &&. Habit.interval h ==. val_ (intervalVal (Proxy @p)))
        pure h

listHabits :: forall p. (HabitTableSelector p, DeadlineCodec p, IntervalTag p) => Connection -> Text -> IO [Habit p]
listHabits conn uid =
  runBeamPostgres conn $
    runSelectReturningList $
      select $ do
        h <- all_ (habitTable @p hitDb)
        guard_ (Habit.user h ==. UserId (val_ uid) &&. Habit.interval h ==. val_ (intervalVal (Proxy @p)))
        pure h

updateHabit :: forall p. (HabitTableSelector p, DeadlineCodec p, IntervalTag p) => Connection -> Text -> UUID.UUID -> Text -> Maybe Text -> Sort -> Fraction -> Deadline p -> IO (Maybe (Habit p))
updateHabit conn uid habitId hname hdesc hsort hrate hdeadline = do
  mExisting <- getHabit @p conn uid habitId
  case mExisting of
    Nothing -> pure Nothing
    Just _ -> do
      runBeamPostgres
        conn
        ( runUpdate
            ( update
                (habitTable @p hitDb)
                ( \h ->
                    mconcat
                      [ Habit.name h <-. val_ hname,
                        Habit.description h <-. val_ hdesc,
                        Habit.interval h <-. val_ (intervalVal (Proxy @p)),
                        Habit.sort h <-. val_ (PgJSON hsort),
                        Habit.rate h <-. val_ (PgJSON hrate),
                        Habit.deadline h <-. val_ hdeadline
                      ]
                )
                (\h -> Habit.id h ==. val_ habitId &&. Habit.user h ==. UserId (val_ uid) &&. Habit.interval h ==. val_ (intervalVal (Proxy @p)))
            )
        )
      pure (Just (Habit habitId (UserId uid) hname hdesc (intervalVal (Proxy @p)) (PgJSON hsort) (PgJSON hrate) hdeadline))

deleteHabit :: forall p. (HabitTableSelector p, DeadlineCodec p, IntervalTag p) => Proxy p -> Connection -> Text -> UUID.UUID -> IO Bool
deleteHabit _ conn uid habitId = do
  mExisting <- getHabit @p conn uid habitId
  case mExisting of
    Nothing -> pure False
    Just _ -> do
      runBeamPostgres conn $
        runDelete $
          delete (habitTable @p hitDb) (\h -> Habit.id h ==. val_ habitId &&. Habit.user h ==. UserId (val_ uid) &&. Habit.interval h ==. val_ (intervalVal (Proxy @p)))
      pure True
