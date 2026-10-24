{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module HIT.Handlers.Habits
  ( habitsServer,
  )
where

import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Maybe (fromMaybe)
import Data.Time (Day, UTCTime, getCurrentTime)
import Data.Traversable (for)
import HIT.API.Habits (HabitRequest (..), HabitResponse (..), HabitsAPI)
import HIT.CRUD (CRUDResource (..), crudServer)
import HIT.DB (createHabit, deleteHabit, getHabit, listHabits, updateHabit)
import HIT.Env (AppM, withDb)
import HIT.Handlers.Users (userToday)
import HIT.Types.Habit (Habit (Habit), SomeHabit (..))
import HIT.Types.Habit qualified as Habit (Habit (..))
import HIT.Types.Identifier (Entity (..), GoalIdentifier, HabitIdentifier, Username, newIdentifier)
import HIT.Types.Interval (Interval (..))
import HIT.Types.Refined (defaultTarget)
import HIT.Types.Schedule (Schedule (..), scheduleInterval)
import HIT.Types.User (User)
import HIT.Types.User qualified as User (User (..))
import HIT.Types.Weekdays (Weekdays, everyDay)
import HIT.Validation (orBadRequest, validateGoalOwnership)
import Servant
import Prelude hiding (id, read)

instance CRUDResource 'HabitEntity where
  type Label 'HabitEntity = "habitId"
  type CreateRequest 'HabitEntity = HabitRequest
  type UpdateRequest 'HabitEntity = HabitRequest
  type Response 'HabitEntity = HabitResponse

  list u =
    withDb $ \conn -> map someHabitResponse <$> listHabits conn u.id

  create u req = do
    start <- maybe (userToday u) pure req.startDate
    goals <- validateGoalOwnership u.id req.goalIds
    identifier <- withDb newIdentifier
    now <- liftIO getCurrentTime
    SomeHabit h <- orBadRequest (habitFromRequest req identifier u.id goals start now now)
    withDb $ \conn -> createHabit conn h
    pure (habitResponse h)

  read u identifier =
    withDb $ \conn -> fmap someHabitResponse <$> getHabit conn u.id identifier

  update u identifier req = do
    mOld <- withDb $ \conn -> getHabit conn u.id identifier
    for mOld $ \(SomeHabit old) -> do
      goals <- validateGoalOwnership u.id req.goalIds
      now <- liftIO getCurrentTime
      SomeHabit h <- orBadRequest (habitFromRequest req identifier u.id goals (fromMaybe old.startDate req.startDate) old.createdAt now)
      withDb $ \conn -> updateHabit conn h
      pure (habitResponse h)

  delete u identifier =
    withDb $ \conn -> deleteHabit conn u.id identifier

habitsServer :: User -> ServerT HabitsAPI AppM
habitsServer = crudServer @'HabitEntity

-- | some schedule is a schedule of either interval
data SomeSchedule where
  SomeSchedule :: Schedule interval -> SomeSchedule

-- | the schedule of an interval and optional weekdays, every day when daily without weekdays
parseSchedule :: Interval -> Maybe Weekdays -> Either String SomeSchedule
parseSchedule Daily days = Right (SomeSchedule (DailySchedule (fromMaybe everyDay days)))
parseSchedule Weekly Nothing = Right (SomeSchedule WeeklySchedule)
parseSchedule Weekly (Just _) = Left "weekdays only apply to daily habits"

-- | the habit a request describes, given its identity, goals, start and timestamps
habitFromRequest :: HabitRequest -> HabitIdentifier -> Username -> NonEmpty GoalIdentifier -> Day -> UTCTime -> UTCTime -> Either String SomeHabit
habitFromRequest req identifier owner goals start created modified = do
  unless (maybe True (>= start) req.endDate) (Left "endDate must not be before startDate")
  SomeSchedule s <- parseSchedule req.interval req.weekdays
  pure (SomeHabit (Habit identifier owner req.title (fromMaybe defaultTarget req.target) req.unit s req.dueTime start req.endDate goals created modified))

someHabitResponse :: SomeHabit -> HabitResponse
someHabitResponse (SomeHabit h) = habitResponse h

habitResponse :: Habit interval -> HabitResponse
habitResponse h =
  HabitResponse h.id h.title h.target h.unit (scheduleInterval h.schedule) (weekdaysOf h.schedule) h.dueTime h.startDate h.endDate (NE.toList h.goals) h.createdAt h.modifiedAt
  where
    weekdaysOf :: Schedule interval -> Maybe Weekdays
    weekdaysOf (DailySchedule days) = Just days
    weekdaysOf WeeklySchedule = Nothing
