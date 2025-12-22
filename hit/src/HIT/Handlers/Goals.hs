{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module HIT.Handlers.Goals
  ( goalsServer,
    GoalsApi,
  )
where

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
    map toGoalResponse <$> listGoals conn (User.id u)

  create (GoalsResource conn) u (CreateGoalRequest gname gdesc) = do
    gid <- UUIDv4.nextRandom
    toGoalResponse <$> createGoal conn gid (User.id u) gname gdesc

  read (GoalsResource conn) u gid =
    fmap toGoalResponse <$> getGoal conn (User.id u) gid

  update (GoalsResource conn) u gid (UpdateGoalRequest gname gdesc) =
    fmap toGoalResponse <$> updateGoal conn (User.id u) gid gname gdesc

  delete (GoalsResource conn) u =
    deleteGoal conn (User.id u)

goalsServer :: Connection -> User -> Server GoalsApi
goalsServer conn = crudServerTc (GoalsResource conn)

toGoalResponse :: Goal.GoalT Identity -> GoalResponse
toGoalResponse (Goal.Goal gid _ gname gdesc createdAt modifiedAt) =
  GoalResponse (UUID.toText gid) gname gdesc createdAt modifiedAt

type GoalsApi = CrudApiFor GoalsResource
