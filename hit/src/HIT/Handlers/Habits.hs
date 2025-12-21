{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module HIT.Handlers.Habits
  ( habitsServer,
    HabitsApi,
  )
where

import Control.Monad.IO.Class (liftIO)
import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as UUIDv4
import Database.Beam (Identity)
import Database.Beam.Postgres (PgJSON (..))
import Database.PostgreSQL.Simple (Connection)
import HIT.Api.Habits
  ( CreateHabitRequest (..),
    HabitDeadline (..),
    HabitResponse (..),
    HabitView (..),
    HabitsApi,
    UpdateHabitRequest (..),
  )
import HIT.Crud (CrudResource (..), crudServerTc)
import HIT.DB (createHabit, deleteHabit, getHabit, listHabits, updateHabit)
import HIT.DB.Schema (HabitTableSelector)
import HIT.Types.Deadline (DeadlineCodec)
import HIT.Types.Habit qualified as Habit (HabitT (..))
import HIT.Types.Interval (Interval (Daily, Weekly), IntervalTag)
import HIT.Types.User (User)
import HIT.Types.User qualified as User (UserT (..))
import Servant
import Prelude hiding (id, read)

habitsServer :: Connection -> User -> Server HabitsApi
habitsServer conn user =
  listHabitsHandler conn user
    :<|> crudServerTc (HabitsResource @'Daily conn) user
    :<|> crudServerTc (HabitsResource @'Weekly conn) user

listHabitsHandler :: Connection -> User -> Maybe Interval -> Handler [HabitView]
listHabitsHandler conn user mInterval = do
  let uid = User.id user
  case mInterval of
    Just Daily -> liftIO $ map toHabitViewDaily <$> listHabits @'Daily conn uid
    Just Weekly -> liftIO $ map toHabitViewWeekly <$> listHabits @'Weekly conn uid
    Nothing -> do
      daily <- liftIO $ map toHabitViewDaily <$> listHabits @'Daily conn uid
      weekly <- liftIO $ map toHabitViewWeekly <$> listHabits @'Weekly conn uid
      pure (daily <> weekly)

newtype HabitsResource (p :: Interval) = HabitsResource Connection

instance (HabitTableSelector p, IntervalTag p, DeadlineCodec p) => CrudResource (HabitsResource p) where
  type Label (HabitsResource p) = "habitId"
  type InternalId (HabitsResource p) = UUID.UUID
  type CreateReq (HabitsResource p) = CreateHabitRequest p
  type UpdateReq (HabitsResource p) = UpdateHabitRequest p
  type Resp (HabitsResource p) = HabitResponse p

  parseId _ = UUID.fromText

  list (HabitsResource conn) u =
    map toHabitResponse <$> listHabits @p conn (User.id u)

  create (HabitsResource conn) u (CreateHabitRequest hname hdesc hsort hrate hdeadline) = do
    hid <- UUIDv4.nextRandom
    toHabitResponse <$> createHabit @p conn hid (User.id u) hname hdesc hsort hrate hdeadline

  read (HabitsResource conn) u hid =
    fmap toHabitResponse <$> getHabit @p conn (User.id u) hid

  update (HabitsResource conn) u hid (UpdateHabitRequest hname hdesc hsort hrate hdeadline) =
    fmap toHabitResponse <$> updateHabit @p conn (User.id u) hid hname hdesc hsort hrate hdeadline

  delete (HabitsResource conn) u =
    deleteHabit @p Proxy conn (User.id u)

toHabitViewDaily :: Habit.HabitT 'Daily Identity -> HabitView
toHabitViewDaily (Habit.Habit hid _ hname hdesc _ (PgJSON hsort) (PgJSON hrate) hdeadline) =
  HabitView (UUID.toText hid) Daily hname hdesc hsort hrate (DailyDeadline hdeadline)

toHabitViewWeekly :: Habit.HabitT 'Weekly Identity -> HabitView
toHabitViewWeekly (Habit.Habit hid _ hname hdesc _ (PgJSON hsort) (PgJSON hrate) hdeadline) =
  HabitView (UUID.toText hid) Weekly hname hdesc hsort hrate (WeeklyDeadline hdeadline)

toHabitResponse :: Habit.HabitT p Identity -> HabitResponse p
toHabitResponse (Habit.Habit hid _ hname hdesc _ (PgJSON hsort) (PgJSON hrate) hdeadline) =
  HabitResponse (UUID.toText hid) hname hdesc hsort hrate hdeadline
