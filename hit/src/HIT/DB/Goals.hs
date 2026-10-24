{-# LANGUAGE OverloadedRecordDot #-}

module HIT.DB.Goals
  ( createGoal,
    getGoal,
    listGoals,
    updateGoal,
    deleteGoal,
    ensureGoalsBelongToUser,
    GoalLinks,
    withLinks,
    setLinks,
  )
where

import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Time (Day, getCurrentTime)
import Data.Traversable (for)
import Database.Beam
import Database.Beam.Backend.SQL.BeamExtensions (runDeleteReturningList)
import Database.Beam.Postgres (Postgres, runBeamPostgres)
import Database.PostgreSQL.Simple (Connection)
import HIT.DB.Codec (decodeGoal, decodeOrThrow, encodeGoal)
import HIT.DB.Rows (GoalLinkT (GoalLink), PrimaryKey (UserRowId))
import HIT.DB.Rows qualified as Row (GoalLinkT (..), GoalRowT (..))
import HIT.DB.Schema (HITDb (..), hitDb)
import HIT.Types.Goal (Goal (..))
import HIT.Types.Identifier (GoalIdentifier, Identifier, Username)
import HIT.Types.Refined (Description, GoalNumber, Title, refine, unrefine)

createGoal :: Connection -> GoalIdentifier -> Username -> Title -> Description -> Text -> Day -> Maybe Day -> IO Goal
createGoal conn identifier owner gname gdescription gcolor start end = do
  now <- getCurrentTime
  gnumber <- nextGoalNumber conn owner
  let g = Goal identifier owner gname gdescription gnumber gcolor start end now now
  runBeamPostgres conn $ runInsert $ insert (goals hitDb) $ insertValues [encodeGoal g]
  pure g

getGoal :: Connection -> Username -> GoalIdentifier -> IO (Maybe Goal)
getGoal conn owner identifier = do
  row <-
    runBeamPostgres conn $
      runSelectReturningOne $
        select $ do
          g <- all_ (goals hitDb)
          guard_ (Row.id g ==. val_ identifier &&. Row.user g ==. UserRowId (val_ owner))
          pure g
  traverse (decodeOrThrow . decodeGoal) row

-- | by number
listGoals :: Connection -> Username -> IO [Goal]
listGoals conn owner = do
  rows <-
    runBeamPostgres conn $
      runSelectReturningList $
        select $
          orderBy_ (asc_ . Row.number) $ do
            g <- all_ (goals hitDb)
            guard_ (Row.user g ==. UserRowId (val_ owner))
            pure g
  traverse (decodeOrThrow . decodeGoal) rows

updateGoal :: Connection -> Username -> GoalIdentifier -> Title -> Description -> Text -> Day -> Maybe Day -> IO (Maybe Goal)
updateGoal conn owner identifier gname gdescription gcolor start end = do
  mOld <- getGoal conn owner identifier
  for mOld $ \old -> do
    now <- getCurrentTime
    let g = old {name = gname, description = gdescription, color = gcolor, startDate = start, endDate = end, modifiedAt = now}
    runBeamPostgres conn $ runUpdate $ save (goals hitDb) (encodeGoal g)
    pure g

deleteGoal :: Connection -> Username -> GoalIdentifier -> IO Bool
deleteGoal conn owner identifier =
  fmap (not . null)
    $ runBeamPostgres conn
    $ runDeleteReturningList
    $ delete (goals hitDb) (\g -> Row.id g ==. val_ identifier &&. Row.user g ==. UserRowId (val_ owner))

-- | smallest positive goal number not in use by the user
nextGoalNumber :: Connection -> Username -> IO GoalNumber
nextGoalNumber conn owner = do
  used <- map (unrefine . (.number)) <$> listGoals conn owner
  either (fail . show) pure (refine (until (`notElem` used) (+ 1) 1))

-- * goal links

ensureGoalsBelongToUser :: Connection -> Username -> NonEmpty GoalIdentifier -> IO Bool
ensureGoalsBelongToUser conn uid goalIds = do
  matches <-
    runBeamPostgres conn $
      runSelectReturningList $
        select $ do
          g <- all_ (goals hitDb)
          guard_ (Row.id g `in_` map val_ (NE.toList goalIds) &&. Row.user g ==. UserRowId (val_ uid))
          pure (Row.id g)
  pure (length matches == length goalIds)

-- | habit_goals or intention_goals, by owner entity
type GoalLinks entity = DatabaseEntity Postgres HITDb (TableEntity (GoalLinkT entity))

-- | pair each row with its goal ids, one query
withLinks :: (Typeable entity) => GoalLinks entity -> Connection -> (a -> Identifier entity) -> [a] -> IO [(a, [GoalIdentifier])]
withLinks _ _ _ [] = pure []
withLinks tbl conn key xs = do
  rows <-
    runBeamPostgres conn $
      runSelectReturningList $
        select $ do
          l <- all_ tbl
          guard_ (Row.owner l `in_` map (val_ . key) xs)
          pure l
  let byOwner = Map.fromListWith (<>) [(o, [g]) | GoalLink o g <- rows]
  pure [(x, Map.findWithDefault [] (key x) byOwner) | x <- xs]

-- | replace the goal ids of one row
setLinks :: (Typeable entity) => GoalLinks entity -> Connection -> Identifier entity -> NonEmpty GoalIdentifier -> IO ()
setLinks tbl conn o goalIds =
  runBeamPostgres conn $ do
    runDelete $ delete tbl (\l -> Row.owner l ==. val_ o)
    runInsert $ insert tbl $ insertValues (map (GoalLink o) (NE.toList goalIds))
