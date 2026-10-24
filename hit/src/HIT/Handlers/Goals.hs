{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module HIT.Handlers.Goals
  ( goalsServer,
  )
where

import Control.Exception (throwIO, try)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Data.Time (Day)
import Database.PostgreSQL.Simple qualified as PG
import HIT.API.Goals (CreateGoalRequest (..), GoalResponse (..), GoalsAPI, UpdateGoalRequest (..))
import HIT.CRUD (CRUDResource (..), crudServer)
import HIT.DB (createGoal, deleteGoal, getGoal, listGoals, updateGoal)
import HIT.Env (AppM, withDb)
import HIT.Handlers.Users (userToday)
import HIT.Types.Goal (Goal)
import HIT.Types.Goal qualified as Goal (Goal (..))
import HIT.Types.Identifier (Entity (..), newIdentifier)
import HIT.Types.User (User)
import HIT.Types.User qualified as User (User (..))
import HIT.Validation (orBadRequest)
import Servant
import Prelude hiding (id, read)

instance CRUDResource 'GoalEntity where
  type Label 'GoalEntity = "goalId"
  type CreateRequest 'GoalEntity = CreateGoalRequest
  type UpdateRequest 'GoalEntity = UpdateGoalRequest
  type Response 'GoalEntity = GoalResponse

  list u =
    withDb (\conn -> listGoals conn (User.id u)) >>= traverse goalResponse

  create u (CreateGoalRequest gname gdesc gcolor gstartDate gendDate) = do
    start <- maybe (userToday u) pure gstartDate
    orBadRequest (checkPeriod start gendDate)
    gid <- withDb newIdentifier
    res <- withDb $ \conn -> try (createGoal conn gid (User.id u) gname gdesc gcolor start gendDate)
    case res of
      Left e | PG.sqlState e == "23505" -> throwError err409 {errBody = "Goal number conflict, retry"}
      Left e -> liftIO (throwIO e)
      Right g -> goalResponse g

  read u gid =
    withDb (\conn -> getGoal conn (User.id u) gid) >>= traverse goalResponse

  update u gid (UpdateGoalRequest gname gdesc gcolor gstartDate gendDate) = do
    orBadRequest (checkPeriod gstartDate gendDate)
    withDb (\conn -> updateGoal conn (User.id u) gid gname gdesc gcolor gstartDate gendDate) >>= traverse goalResponse

  -- linked goals are kept, so habits and intentions keep a goal
  delete u gid = do
    res <- withDb $ \conn -> try (deleteGoal conn (User.id u) gid)
    case res of
      Left e | PG.sqlState e == "23503" -> throwError err409 {errBody = "Goal still has habits or intentions"}
      Left e -> liftIO (throwIO e)
      Right deleted -> pure deleted

goalsServer :: User -> ServerT GoalsAPI AppM
goalsServer = crudServer @'GoalEntity

-- | ends after it starts
checkPeriod :: Day -> Maybe Day -> Either String ()
checkPeriod start end = unless (maybe True (> start) end) (Left "endDate must be after startDate")

goalResponse :: Goal -> AppM GoalResponse
goalResponse g =
  pure (GoalResponse g.id g.number g.name g.description g.color g.startDate g.endDate g.createdAt g.modifiedAt)
