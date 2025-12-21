{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module HIT.DB.Goals
  ( createGoal,
    getGoal,
    listGoals,
    updateGoal,
    deleteGoal,
  )
where

import Data.Text (Text)
import Data.UUID qualified as UUID
import Database.Beam
import Database.Beam.Sqlite (runBeamSqlite)
import Database.SQLite.Simple (Connection)
import HIT.DB.Schema (HITDb (..), hitDb)
import HIT.Types.Goal (Goal, GoalT (..))
import HIT.Types.Goal qualified as Goal (GoalT (..))
import HIT.Types.User (PrimaryKey (UserId))

createGoal :: Connection -> UUID.UUID -> Text -> Text -> Maybe Text -> IO Goal
createGoal conn goalId uid gname gdesc = do
  let g = Goal goalId (UserId uid) gname gdesc
  runBeamSqlite conn $
    runInsert $
      insert (goals hitDb) $
        insertValues [g]
  pure g

getGoal :: Connection -> Text -> UUID.UUID -> IO (Maybe Goal)
getGoal conn uid goalId =
  runBeamSqlite conn $
    runSelectReturningOne $
      select $ do
        g <- all_ (goals hitDb)
        guard_ (Goal.id g ==. val_ goalId &&. Goal.user g ==. UserId (val_ uid))
        pure g

listGoals :: Connection -> Text -> IO [Goal]
listGoals conn uid =
  runBeamSqlite conn $
    runSelectReturningList $
      select $ do
        g <- all_ (goals hitDb)
        guard_ (Goal.user g ==. UserId (val_ uid))
        pure g

updateGoal :: Connection -> Text -> UUID.UUID -> Text -> Maybe Text -> IO (Maybe Goal)
updateGoal conn uid goalId gname gdesc = do
  mExisting <- getGoal conn uid goalId
  case mExisting of
    Nothing -> pure Nothing
    Just _ -> do
      runBeamSqlite conn $
        runUpdate $
          update
            (goals hitDb)
            (\g -> mconcat [Goal.name g <-. val_ gname, Goal.description g <-. val_ gdesc])
            (\g -> Goal.id g ==. val_ goalId &&. Goal.user g ==. UserId (val_ uid))
      pure (Just (Goal goalId (UserId uid) gname gdesc))

deleteGoal :: Connection -> Text -> UUID.UUID -> IO Bool
deleteGoal conn uid goalId = do
  mExisting <- getGoal conn uid goalId
  case mExisting of
    Nothing -> pure False
    Just _ -> do
      runBeamSqlite conn $
        runDelete $
          delete (goals hitDb) (\g -> Goal.id g ==. val_ goalId &&. Goal.user g ==. UserId (val_ uid))
      pure True
