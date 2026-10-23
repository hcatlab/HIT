{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module HIT.Handlers.Goals
  ( goalsServer,
    GoalsApi,
  )
where

import Control.Monad.IO.Class (liftIO)
import Data.UUID (UUID)
import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as UUIDv4
import Database.Beam (Identity)
import Database.PostgreSQL.Simple (Connection)
import HIT.Api.Goals (CreateGoalRequest (..), GoalResponse (..), UpdateGoalRequest (..))
import HIT.Crud (CrudApiFor, CrudResource (..), crudServerTc)
import HIT.DB (createGoal, deleteGoal, getGoal, listGoals, updateGoal)
import HIT.Types.Goal qualified as Goal (GoalT (..))
import HIT.Types.User (User)
import HIT.Types.User qualified as User (UserT (..))
import HIT.Validation (requireNonEmpty)
import Servant
import Prelude hiding (id, read)

newtype GoalsResource = GoalsResource Connection

instance CrudResource GoalsResource where
  type Label GoalsResource = "goalId"
  type InternalId GoalsResource = UUID
  type CreateReq GoalsResource = CreateGoalRequest
  type UpdateReq GoalsResource = UpdateGoalRequest
  type Resp GoalsResource = GoalResponse

  parseId _ = UUID.fromText

  list (GoalsResource conn) u =
    liftIO $ map toGoalResponse <$> listGoals conn (User.id u)

  create (GoalsResource conn) u (CreateGoalRequest gname gdesc gcolor gstartDate gendDate) = do
    _ <- requireNonEmpty "name" gname
    _ <- requireNonEmpty "description" gdesc
    gid <- liftIO UUIDv4.nextRandom
    liftIO $ toGoalResponse <$> createGoal conn gid (User.id u) gname gdesc gcolor gstartDate gendDate

  read (GoalsResource conn) u gid =
    liftIO $ fmap toGoalResponse <$> getGoal conn (User.id u) gid

  update (GoalsResource conn) u gid (UpdateGoalRequest gname gdesc gcolor gstartDate gendDate) = do
    _ <- requireNonEmpty "name" gname
    _ <- requireNonEmpty "description" gdesc
    liftIO $ fmap toGoalResponse <$> updateGoal conn (User.id u) gid gname gdesc gcolor gstartDate gendDate

  delete (GoalsResource conn) u gid =
    liftIO $ deleteGoal conn (User.id u) gid

goalsServer :: Connection -> User -> Server GoalsApi
goalsServer conn = crudServerTc (GoalsResource conn)

toGoalResponse :: Goal.GoalT Identity -> GoalResponse
toGoalResponse (Goal.Goal gid _ gname gdesc num gcolor gstartDate gendDate createdAt modifiedAt) =
  GoalResponse (UUID.toText gid) (fromIntegral num :: Int) gname gdesc gcolor gstartDate gendDate createdAt modifiedAt

type GoalsApi = CrudApiFor GoalsResource
