{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RankNTypes #-}

-- | rows to domain values and back, the only place stored data is checked
module HIT.DB.Codec
  ( DecodeError (..),
    encodeHabit,
    decodeHabit,
    encodeIntention,
    decodeIntention,
    encodeUser,
    decodeUser,
    encodeGoal,
    decodeGoal,
    decodeOrThrow,
  )
where

import Control.Exception (Exception, displayException, throwIO)
import Control.Monad (when)
import Data.Bifunctor (first)
import Data.List.NonEmpty (nonEmpty)
import Data.Maybe (isJust)
import Data.Time (Day)
import HIT.DB.DateRange (dateRange, rangeDays)
import HIT.DB.Rows (GoalRow, GoalRowT (..), HabitRow, HabitRowT (..), IntentionRow, IntentionRowT (..), PrimaryKey (UserRowId), UserRow, UserRowT (..))
import HIT.Types.Goal (Goal (..))
import HIT.Types.Habit (Habit (..), SomeHabit (..))
import HIT.Types.Identifier (GoalIdentifier, HabitIdentifier)
import HIT.Types.Intention (Intention (..), SomeIntention (..), Source (..))
import HIT.Types.Interval (Interval (..))
import HIT.Types.Refined (Predicate, Refined, refine, unrefine)
import HIT.Types.Schedule (Schedule (..), Window (..), isoWeekBounds, isoWeekOf, scheduleInterval, windowBounds, windowInterval)
import HIT.Types.User (User (..))
import HIT.Types.Weekdays (Weekdays)

-- | a decode error happens when a stored row breaks a domain invariant
data DecodeError
  = -- | no goal links
    NoGoals
  | -- | weekdays present iff not daily
    ScheduleMismatch Interval
  | -- | dates not a window of the interval
    WindowMismatch Interval Day Day
  | -- | doneAt present iff progress below target
    DoneMismatch
  | -- | a range without a first day or, for windows, a last day
    RangeMismatch
  | -- | a column outside its refinement
    RefinementFailed String
  deriving stock (Show, Eq)

instance Exception DecodeError

encodeHabit :: Habit interval -> HabitRow
encodeHabit h =
  HabitRow h.id (UserRowId h.owner) (unrefine h.title) (unrefine h.target) h.unit (scheduleInterval h.schedule) (scheduleWeekdays h.schedule) h.dueTime (dateRange h.startDate h.endDate) h.createdAt h.modifiedAt

-- | weekdays of a daily schedule
scheduleWeekdays :: Schedule interval -> Maybe Weekdays
scheduleWeekdays (DailySchedule days) = Just days
scheduleWeekdays WeeklySchedule = Nothing

decodeHabit :: HabitRow -> [GoalIdentifier] -> Either DecodeError SomeHabit
decodeHabit r goalIds = do
  goals <- maybe (Left NoGoals) Right (nonEmpty goalIds)
  title <- refined r.title
  target <- refined r.target
  (start, end) <- maybe (Left RangeMismatch) Right (rangeDays r.active)
  let UserRowId owner = r.user
      habit :: Schedule interval -> Habit interval
      habit s = Habit r.id owner title target r.unit s r.dueTime start end goals r.createdAt r.modifiedAt
  case (r.interval, r.weekdays) of
    (Daily, Just days) -> Right (SomeHabit (habit (DailySchedule days)))
    (Weekly, Nothing) -> Right (SomeHabit (habit WeeklySchedule))
    (interval, _) -> Left (ScheduleMismatch interval)

encodeIntention :: Intention origin interval -> IntentionRow
encodeIntention i =
  IntentionRow i.id (UserRowId i.owner) (sourceHabit i.source) (unrefine i.title) (unrefine i.target) i.unit (unrefine i.progress) (windowInterval i.window) (dateRange start (Just due)) i.dueTime i.doneAt i.createdAt i.modifiedAt
  where
    (start, due) = windowBounds i.window

-- | the habit of a recurring source
sourceHabit :: Source origin -> Maybe HabitIdentifier
sourceHabit OneOffSource = Nothing
sourceHabit (RecurringSource h) = Just h

decodeIntention :: IntentionRow -> [GoalIdentifier] -> Either DecodeError SomeIntention
decodeIntention r goalIds = do
  goals <- maybe (Left NoGoals) Right (nonEmpty goalIds)
  title <- refined r.title
  target <- refined r.target
  progress <- refined r.progress
  when ((unrefine progress >= unrefine target) /= isJust r.doneAt) (Left DoneMismatch)
  (start, due) <- maybe (Left RangeMismatch) Right (rangeDays r.window >>= sequence)
  let UserRowId owner = r.user
      intention :: Source origin -> Window interval -> Intention origin interval
      intention s w = Intention r.id owner s title target r.unit progress w r.dueTime r.doneAt goals r.createdAt r.modifiedAt
  withWindow r.interval start due (\w -> withSource r (\s -> SomeIntention (intention s w)))

-- | the window of an interval and first and last day, if they fit
withWindow :: Interval -> Day -> Day -> (forall interval. Window interval -> a) -> Either DecodeError a
withWindow interval start due k = case interval of
  Daily | start == due -> Right (k (DailyWindow start))
  Weekly | isoWeekBounds week == (start, due) -> Right (k (WeeklyWindow week))
  _ -> Left (WindowMismatch interval start due)
  where
    week = isoWeekOf start

-- | the source of a row
withSource :: IntentionRow -> (forall origin. Source origin -> a) -> a
withSource r k = maybe (k OneOffSource) (k . RecurringSource) r.habitId

refined :: (Predicate predicate x) => x -> Either DecodeError (Refined predicate x)
refined = first (RefinementFailed . displayException) . refine

encodeUser :: User -> UserRow
encodeUser u = UserRow u.id u.email u.passwordHash u.apiToken u.timezone u.createdAt u.modifiedAt

-- | users have no invariants beyond their row
decodeUser :: UserRow -> Either DecodeError User
decodeUser r = Right (User r.id r.email r.passwordHash r.apiToken r.timezone r.createdAt r.modifiedAt)

encodeGoal :: Goal -> GoalRow
encodeGoal g =
  GoalRow g.id (UserRowId g.owner) (unrefine g.name) (unrefine g.description) (toInteger (unrefine g.number)) g.color (dateRange g.startDate g.endDate) g.createdAt g.modifiedAt

decodeGoal :: GoalRow -> Either DecodeError Goal
decodeGoal r = do
  name <- refined r.name
  description <- refined r.description
  number <- refined (fromInteger r.number)
  (start, end) <- maybe (Left RangeMismatch) Right (rangeDays r.active)
  let UserRowId owner = r.user
  pure (Goal r.id owner name description number r.color start end r.createdAt r.modifiedAt)

-- | decoded rows, a decode error is thrown as a 500
decodeOrThrow :: Either DecodeError a -> IO a
decodeOrThrow = either throwIO pure
