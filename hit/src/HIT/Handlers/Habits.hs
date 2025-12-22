{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module HIT.Handlers.Habits
  ( habitsServer,
    HabitsApi,
  )
where

import Control.Exception (throwIO)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString.Lazy.Char8 qualified as LBS
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Text (Text)
import Data.UUID (UUID)
import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as UUIDv4
import Database.Beam (Identity)
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
import HIT.DB
  ( createHabit,
    deleteHabit,
    ensureGoalsBelongToUser,
    getHabitWithGoals,
    listHabitsWithGoals,
    updateHabit,
  )
import HIT.DB.Schema (HabitTableSelector)
import HIT.Types.Deadline (DeadlineJson)
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
    Just Daily -> liftIO $ map (uncurry toHabitViewDaily) <$> listHabitsWithGoals @'Daily conn uid
    Just Weekly -> liftIO $ map (uncurry toHabitViewWeekly) <$> listHabitsWithGoals @'Weekly conn uid
    Nothing -> do
      daily <- liftIO $ map (uncurry toHabitViewDaily) <$> listHabitsWithGoals @'Daily conn uid
      weekly <- liftIO $ map (uncurry toHabitViewWeekly) <$> listHabitsWithGoals @'Weekly conn uid
      pure (daily <> weekly)

newtype HabitsResource (p :: Interval) = HabitsResource Connection

instance (HabitTableSelector p, IntervalTag p, DeadlineJson p) => CrudResource (HabitsResource p) where
  type Label (HabitsResource p) = "habitId"
  type InternalId (HabitsResource p) = UUID
  type CreateReq (HabitsResource p) = CreateHabitRequest p
  type UpdateReq (HabitsResource p) = UpdateHabitRequest p
  type Resp (HabitsResource p) = HabitResponse p

  parseId _ = UUID.fromText

  list (HabitsResource conn) u = do
    map toHabitResponseWithGoals <$> listHabitsWithGoals @p conn (User.id u)

  create (HabitsResource conn) u (CreateHabitRequest hname hdesc hsort hrate hdeadline goalIdsText) = do
    goalIds <- parseGoalIds goalIdsText
    ensureValidGoals conn (User.id u) goalIds
    hid <- liftIO UUIDv4.nextRandom
    habit <- liftIO $ createHabit @p conn hid (User.id u) hname hdesc hsort hrate hdeadline goalIds
    pure (toHabitResponseWithGoals (habit, NE.toList goalIds))

  read (HabitsResource conn) u hid =
    fmap toHabitResponseWithGoals <$> getHabitWithGoals @p conn (User.id u) hid

  update (HabitsResource conn) u hid (UpdateHabitRequest hname hdesc hsort hrate hdeadline goalIdsText) = do
    goalIds <- parseGoalIds goalIdsText
    ensureValidGoals conn (User.id u) goalIds
    fmap (toHabitResponseWithGoals . (,NE.toList goalIds)) <$> updateHabit @p conn (User.id u) hid hname hdesc hsort hrate hdeadline goalIds

  delete (HabitsResource conn) u =
    deleteHabit @p Proxy conn (User.id u)

toHabitViewDaily :: Habit.HabitT 'Daily Identity -> [UUID] -> HabitView
toHabitViewDaily (Habit.Habit hid _ hname hdesc _ hsort hrate hdeadline) goalIds =
  HabitView (UUID.toText hid) Daily hname hdesc hsort hrate (DailyDeadline hdeadline) (map UUID.toText goalIds)

toHabitViewWeekly :: Habit.HabitT 'Weekly Identity -> [UUID] -> HabitView
toHabitViewWeekly (Habit.Habit hid _ hname hdesc _ hsort hrate hdeadline) goalIds =
  HabitView (UUID.toText hid) Weekly hname hdesc hsort hrate (WeeklyDeadline hdeadline) (map UUID.toText goalIds)

toHabitResponseWithGoals :: (Habit.HabitT p Identity, [UUID]) -> HabitResponse p
toHabitResponseWithGoals (Habit.Habit hid _ hname hdesc _ hsort hrate hdeadline, goalIds) =
  HabitResponse (UUID.toText hid) hname hdesc hsort hrate hdeadline (map UUID.toText goalIds)

parseGoalIds :: NonEmpty Text -> IO (NonEmpty UUID)
parseGoalIds ids = case traverse UUID.fromText ids of
  Nothing -> throwIO err400 {errBody = LBS.pack "Invalid goal id"}
  Just parsed -> pure parsed

ensureValidGoals :: Connection -> Text -> NonEmpty UUID -> IO ()
ensureValidGoals conn uid goalIds = do
  ok <- ensureGoalsBelongToUser conn uid goalIds
  unless ok $ throwIO err400 {errBody = LBS.pack "goalIds must be non-empty and belong to the current user"}
