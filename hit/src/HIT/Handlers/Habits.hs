{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module HIT.Handlers.Habits
  ( habitsServer,
    HabitsApi,
  )
where

import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as UUIDv4
import Database.Beam (Identity)
import Database.SQLite.Simple (Connection)
import Data.Proxy (Proxy (..))
import HIT.Api.Habits (CreateHabitRequest (..), HabitApiFor, HabitResponse (..), HabitsApi, UpdateHabitRequest (..))
import HIT.Crud (CrudApiFor, CrudResource (..), crudServerTc)
import HIT.DB (createHabit, deleteHabit, getHabit, listHabits, updateHabit)
import HIT.DB.Schema (HabitTableSelector)
import HIT.Types.Deadline (DeadlineCodec)
import HIT.Types.Habit qualified as Habit (HabitT (..))
import HIT.Types.Interval (Interval (Daily, Weekly))
import HIT.Types.User (User)
import HIT.Types.User qualified as User (UserT (..))
import Servant
import Prelude hiding (id, read)

newtype HabitsResource (p :: Interval) = HabitsResource Connection

instance (HabitTableSelector p, DeadlineCodec p) => CrudResource (HabitsResource p) where
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

  delete (HabitsResource conn) u hid =
    deleteHabit @p Proxy conn (User.id u) hid

habitsServer :: Connection -> User -> Server HabitsApi
habitsServer conn user =
  crudServerTc (HabitsResource @'Daily conn) user
    :<|> crudServerTc (HabitsResource @'Weekly conn) user

toHabitResponse :: Habit.HabitT p Identity -> HabitResponse p
toHabitResponse (Habit.Habit hid _ hname hdesc hsort hrate hdeadline) =
  HabitResponse (UUID.toText hid) hname hdesc hsort hrate hdeadline
