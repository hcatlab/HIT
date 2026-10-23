{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module HIT.Handlers.Habits
  ( habitsServer,
    HabitsApi,
  )
where

import Control.Monad.IO.Class (liftIO)
import Data.List.NonEmpty qualified as NE
import Data.UUID (UUID)
import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as UUIDv4
import Database.Beam (Identity)
import HIT.Api.Habits
  ( CreateHabitRequest (..),
    HabitDeadline (..),
    HabitResponse (..),
    HabitView (..),
    HabitsApi,
    UpdateHabitRequest (..),
  )
import HIT.Crud (CrudResource (..), crudServerTc)
import HIT.DB
  ( createHabit,
    deleteHabit,
    getHabitWithGoals,
    listHabitsWithGoals,
    updateHabit,
  )
import HIT.DB.Schema (HabitTableSelector)
import HIT.Env (AppM, withDb)
import HIT.Types.Deadline (DeadlineJson)
import HIT.Types.Habit qualified as Habit (HabitT (..))
import HIT.Types.Interval (Interval (Daily, Weekly), IntervalTag)
import HIT.Types.User (User)
import HIT.Types.User qualified as User (UserT (..))
import HIT.Validation (parseGoalIds, requireNonEmpty, validateGoalOwnership)
import Servant
import Prelude hiding (id, read)

habitsServer :: User -> ServerT HabitsApi AppM
habitsServer user =
  listHabitsHandler user
    :<|> crudServerTc (HabitsResource @'Daily) user
    :<|> crudServerTc (HabitsResource @'Weekly) user

listHabitsHandler :: User -> Maybe Interval -> AppM [HabitView]
listHabitsHandler user mInterval = do
  let uid = User.id user
  case mInterval of
    Just Daily -> withDb $ \conn -> map (uncurry toHabitViewDaily) <$> listHabitsWithGoals @'Daily conn uid
    Just Weekly -> withDb $ \conn -> map (uncurry toHabitViewWeekly) <$> listHabitsWithGoals @'Weekly conn uid
    Nothing -> do
      daily <- withDb $ \conn -> map (uncurry toHabitViewDaily) <$> listHabitsWithGoals @'Daily conn uid
      weekly <- withDb $ \conn -> map (uncurry toHabitViewWeekly) <$> listHabitsWithGoals @'Weekly conn uid
      pure (daily <> weekly)

data HabitsResource (p :: Interval) = HabitsResource

instance (HabitTableSelector p, IntervalTag p, DeadlineJson p) => CrudResource (HabitsResource p) where
  type Label (HabitsResource p) = "habitId"
  type InternalId (HabitsResource p) = UUID
  type CreateReq (HabitsResource p) = CreateHabitRequest p
  type UpdateReq (HabitsResource p) = UpdateHabitRequest p
  type Resp (HabitsResource p) = HabitResponse p

  parseId _ = UUID.fromText

  list _ u =
    withDb $ \conn -> map toHabitResponseWithGoals <$> listHabitsWithGoals @p conn (User.id u)

  create _ u (CreateHabitRequest hname hdesc hsort hrate hdeadline goalIdsText) = do
    _ <- requireNonEmpty "name" hname
    goalIds <- parseGoalIds goalIdsText
    validateGoalOwnership (User.id u) goalIds
    hid <- liftIO UUIDv4.nextRandom
    habit <- withDb $ \conn -> createHabit @p conn hid (User.id u) hname hdesc hsort hrate hdeadline goalIds
    pure (toHabitResponseWithGoals (habit, NE.toList goalIds))

  read _ u hid =
    withDb $ \conn -> fmap toHabitResponseWithGoals <$> getHabitWithGoals @p conn (User.id u) hid

  update _ u hid (UpdateHabitRequest hname hdesc hsort hrate hdeadline goalIdsText) = do
    _ <- requireNonEmpty "name" hname
    goalIds <- parseGoalIds goalIdsText
    validateGoalOwnership (User.id u) goalIds
    withDb $ \conn -> fmap (toHabitResponseWithGoals . (,NE.toList goalIds)) <$> updateHabit @p conn (User.id u) hid hname hdesc hsort hrate hdeadline goalIds

  delete _ u hid =
    withDb $ \conn -> deleteHabit @p Proxy conn (User.id u) hid

toHabitViewDaily :: Habit.HabitT 'Daily Identity -> [UUID] -> HabitView
toHabitViewDaily (Habit.Habit hid _ hname hdesc _ hsort hrate hdeadline createdAt modifiedAt) goalIds =
  HabitView (UUID.toText hid) Daily hname hdesc hsort hrate (DailyDeadline hdeadline) (map UUID.toText goalIds) createdAt modifiedAt

toHabitViewWeekly :: Habit.HabitT 'Weekly Identity -> [UUID] -> HabitView
toHabitViewWeekly (Habit.Habit hid _ hname hdesc _ hsort hrate hdeadline createdAt modifiedAt) goalIds =
  HabitView (UUID.toText hid) Weekly hname hdesc hsort hrate (WeeklyDeadline hdeadline) (map UUID.toText goalIds) createdAt modifiedAt

toHabitResponseWithGoals :: (Habit.HabitT p Identity, [UUID]) -> HabitResponse p
toHabitResponseWithGoals (Habit.Habit hid _ hname hdesc _ hsort hrate hdeadline createdAt modifiedAt, goalIds) =
  HabitResponse (UUID.toText hid) hname hdesc hsort hrate hdeadline (map UUID.toText goalIds) createdAt modifiedAt
