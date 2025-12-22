{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module HIT.DB.Habits
  ( createHabit,
    getHabit,
    getHabitWithGoals,
    listHabits,
    listHabitsWithGoals,
    updateHabit,
    deleteHabit,
    ensureGoalsBelongToUser,
    listHabitGoals,
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
import HIT.DB.Schema (HabitGoalT (..), HabitTableSelector (..), goals, habitGoals, hitDb)
import HIT.Types.Deadline (Deadline, DeadlineJson)
import HIT.Types.Fraction (Fraction)
import HIT.Types.Goal qualified as Goal
import HIT.Types.Habit (Habit, HabitT (..))
import HIT.Types.Habit qualified as Habit (HabitT (..))
import HIT.Types.Interval (IntervalTag (..))
import HIT.Types.Sort (Sort)
import HIT.Types.User (PrimaryKey (UserId))

createHabit :: forall p. (HabitTableSelector p, DeadlineJson p, IntervalTag p) => Connection -> UUID -> Text -> Text -> Maybe Text -> Sort -> Fraction -> Deadline p -> NonEmpty UUID -> IO (Habit p)
createHabit conn habitId uid hname hdesc hsort hrate hdeadline goalIds = do
  let h = Habit habitId (UserId uid) hname hdesc (intervalVal (Proxy @p)) hsort hrate hdeadline
  runBeamPostgres conn $
    runInsert $
      insert (habitTable @p hitDb) $
        insertValues [h]
  insertHabitGoals conn habitId goalIds
  pure h

getHabit :: forall p. (HabitTableSelector p, DeadlineJson p, IntervalTag p) => Connection -> Text -> UUID -> IO (Maybe (Habit p))
getHabit conn uid habitId =
  runBeamPostgres conn $
    runSelectReturningOne $
      select $ do
        h <- all_ (habitTable @p hitDb)
        guard_ (Habit.id h ==. val_ habitId &&. Habit.user h ==. UserId (val_ uid) &&. Habit.interval h ==. val_ (intervalVal (Proxy @p)))
        pure h

getHabitWithGoals :: forall p. (HabitTableSelector p, DeadlineJson p, IntervalTag p) => Connection -> Text -> UUID -> IO (Maybe (Habit p, [UUID]))
getHabitWithGoals conn uid habitId = do
  mHabit <- getHabit @p conn uid habitId
  case mHabit of
    Nothing -> pure Nothing
    Just h -> do
      goals <- listHabitGoals conn uid habitId
      pure (Just (h, goals))

listHabits :: forall p. (HabitTableSelector p, DeadlineJson p, IntervalTag p) => Connection -> Text -> IO [Habit p]
listHabits conn uid =
  runBeamPostgres conn $
    runSelectReturningList $
      select $ do
        h <- all_ (habitTable @p hitDb)
        guard_ (Habit.user h ==. UserId (val_ uid) &&. Habit.interval h ==. val_ (intervalVal (Proxy @p)))
        pure h

listHabitsWithGoals :: forall p. (HabitTableSelector p, DeadlineJson p, IntervalTag p) => Connection -> Text -> IO [(Habit p, [UUID])]
listHabitsWithGoals conn uid = do
  hs <- listHabits @p conn uid
  mapM (\t -> (t,) <$> listHabitGoals conn uid (Habit.id t)) hs

updateHabit :: forall p. (HabitTableSelector p, DeadlineJson p, IntervalTag p) => Connection -> Text -> UUID -> Text -> Maybe Text -> Sort -> Fraction -> Deadline p -> NonEmpty UUID -> IO (Maybe (Habit p))
updateHabit conn uid habitId hname hdesc hsort hrate hdeadline goalIds = do
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
                        Habit.sort h <-. val_ hsort,
                        Habit.rate h <-. val_ hrate,
                        Habit.deadline h <-. val_ hdeadline
                      ]
                )
                (\h -> Habit.id h ==. val_ habitId &&. Habit.user h ==. UserId (val_ uid) &&. Habit.interval h ==. val_ (intervalVal (Proxy @p)))
            )
        )
      replaceHabitGoals conn habitId goalIds
      pure (Just (Habit habitId (UserId uid) hname hdesc (intervalVal (Proxy @p)) hsort hrate hdeadline))

deleteHabit :: forall p. (HabitTableSelector p, DeadlineJson p, IntervalTag p) => Proxy p -> Connection -> Text -> UUID -> IO Bool
deleteHabit _ conn uid habitId = do
  mExisting <- getHabit @p conn uid habitId
  case mExisting of
    Nothing -> pure False
    Just _ -> do
      runBeamPostgres conn $
        runDelete $
          delete (habitTable @p hitDb) (\h -> Habit.id h ==. val_ habitId &&. Habit.user h ==. UserId (val_ uid) &&. Habit.interval h ==. val_ (intervalVal (Proxy @p)))
      pure True

-- Goal mapping helpers

ensureGoalsBelongToUser :: Connection -> Text -> NonEmpty UUID -> IO Bool
ensureGoalsBelongToUser conn uid goalIds = do
  matches <-
    runBeamPostgres
      conn
      ( runSelectReturningList
          ( select $ do
              g <- all_ (goals hitDb)
              guard_ (Goal.id g `in_` map val_ (NE.toList goalIds) &&. Goal.user g ==. UserId (val_ uid))
              pure (Goal.id g)
          )
      )
  pure (length matches == length (NE.toList goalIds))

insertHabitGoals :: Connection -> UUID -> NonEmpty UUID -> IO ()
insertHabitGoals conn hid goalIds =
  runBeamPostgres conn $
    runInsert $
      insert (habitGoals hitDb) $
        insertValues (NE.toList (fmap (HabitGoal hid) goalIds))

replaceHabitGoals :: Connection -> UUID -> NonEmpty UUID -> IO ()
replaceHabitGoals conn hid goalIds = do
  runBeamPostgres conn $
    runDelete $
      delete (habitGoals hitDb) (\hg -> habitId hg ==. val_ hid)
  insertHabitGoals conn hid goalIds

listHabitGoals :: Connection -> Text -> UUID -> IO [UUID]
listHabitGoals conn uid hid =
  runBeamPostgres conn $
    runSelectReturningList $
      select $ do
        hg <- all_ (habitGoals hitDb)
        g <- all_ (goals hitDb)
        guard_ (habitId hg ==. val_ hid &&. goalId hg ==. Goal.id g &&. Goal.user g ==. UserId (val_ uid))
        pure (Goal.id g)
