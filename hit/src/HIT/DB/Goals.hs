module HIT.DB.Goals
  ( createGoal,
    getGoal,
    listGoals,
    updateGoal,
    deleteGoal,
    getNextGoalNumber,
  )
where

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Time (Day, UTCTime, getCurrentTime, utctDay)
import Data.UUID (UUID)
import Database.Beam
import Database.Beam.Postgres (runBeamPostgres)
import Database.PostgreSQL.Simple (Connection)
import HIT.DB.Schema (HITDb (..), hitDb)
import HIT.Types.Goal (Goal, GoalT (..))
import HIT.Types.Goal qualified as Goal (GoalT (..))
import HIT.Types.User (PrimaryKey (UserId))

createGoal :: Connection -> UUID -> Text -> Text -> Text -> Text -> Maybe Day -> Maybe Day -> IO Goal
createGoal conn goalId uid gname gdesc gcolor gstartDate gendDate = do
  now <- getCurrentTime
  nextNum <- getNextGoalNumber conn uid
  let effectiveStart = fromMaybe (utctDay now) gstartDate
  let g = Goal goalId (UserId uid) gname gdesc (fromIntegral nextNum :: Integer) gcolor effectiveStart gendDate now now
  runBeamPostgres conn $
    runInsert $
      insert (goals hitDb) $
        insertValues [g]
  pure g

getGoal :: Connection -> Text -> UUID -> IO (Maybe Goal)
getGoal conn uid goalId =
  runBeamPostgres conn $
    runSelectReturningOne $
      select $ do
        g <- all_ (goals hitDb)
        guard_ (Goal.id g ==. val_ goalId &&. Goal.user g ==. UserId (val_ uid))
        pure g

listGoals :: Connection -> Text -> IO [Goal]
listGoals conn uid =
  runBeamPostgres conn $
    runSelectReturningList $
      select $ do
        g <- all_ (goals hitDb)
        guard_ (Goal.user g ==. UserId (val_ uid))
        pure g

updateGoal :: Connection -> Text -> UUID -> Text -> Text -> Text -> Day -> Maybe Day -> IO (Maybe Goal)
updateGoal conn uid goalId gname gdesc gcolor gstartDate gendDate = do
  mExisting <- getGoal conn uid goalId
  case mExisting of
    Nothing -> pure Nothing
    Just (Goal _ _ _ _ _ _ _ _ _ _) -> do
      now <- getCurrentTime
      runBeamPostgres conn $
        runUpdate $
          update
            (goals hitDb)
            (\g -> mconcat [Goal.name g <-. val_ gname, Goal.description g <-. val_ gdesc, Goal.color g <-. val_ gcolor, Goal.startDate g <-. val_ gstartDate, Goal.endDate g <-. val_ gendDate, Goal.modifiedAt g <-. val_ now])
            (\g -> Goal.id g ==. val_ goalId &&. Goal.user g ==. UserId (val_ uid))
      -- Re-fetch to get the current goal number
      getGoal conn uid goalId

deleteGoal :: Connection -> Text -> UUID -> IO Bool
deleteGoal conn uid goalId = do
  mExisting <- getGoal conn uid goalId
  case mExisting of
    Nothing -> pure False
    Just _ -> do
      runBeamPostgres conn $
        runDelete $
          delete (goals hitDb) (\g -> Goal.id g ==. val_ goalId &&. Goal.user g ==. UserId (val_ uid))
      pure True

-- Get next available goal number for a user.
-- Returns the smallest positive integer not currently in use by active (non-archived) goals.
getNextGoalNumber :: Connection -> Text -> IO Int
getNextGoalNumber conn uid = do
  goals_ <- listGoals conn uid
  let usedNumbers = map (\(Goal _ _ _ _ num _ _ _ _ _) -> fromIntegral num :: Int) goals_
  pure $ findNextAvailable 1 usedNumbers
  where
    findNextAvailable num used
      | num `notElem` used = num
      | otherwise = findNextAvailable (num + 1) used
