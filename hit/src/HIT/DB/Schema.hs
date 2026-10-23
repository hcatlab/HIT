{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module HIT.DB.Schema where

import Data.Time (UTCTime)
import Data.UUID (UUID)
import Database.Beam
import Database.Beam.Migrate
import Database.Beam.Postgres (Postgres)
import Database.Beam.Postgres.Migrate qualified as Pg
import Database.Beam.Postgres.Syntax (pgJsonType)
import HIT.Types.Goal (GoalT (..))
import HIT.Types.Habit (HabitT (..))
import HIT.Types.Intention (IntentionT (..))
import HIT.Types.Interval (Interval (..))
import HIT.Types.User (PrimaryKey (UserId), UserT (..))

data HITDb f = HITDb
  { users :: f (TableEntity UserT),
    goals :: f (TableEntity GoalT),
    habitsDaily :: f (TableEntity (HabitT 'Daily)),
    habitsWeekly :: f (TableEntity (HabitT 'Weekly)),
    intentionsDaily :: f (TableEntity (IntentionT 'Daily)),
    intentionsWeekly :: f (TableEntity (IntentionT 'Weekly)),
    habitGoals :: f (TableEntity HabitGoalT),
    intentionGoals :: f (TableEntity IntentionGoalT)
  }
  deriving (Generic)

instance Database Postgres HITDb

-- * goal mapping tables

data HabitGoalT f = HabitGoal
  { habitId :: Columnar f UUID,
    goalId :: Columnar f UUID
  }
  deriving (Generic)

type HabitGoal = HabitGoalT Identity

instance Beamable HabitGoalT

instance Table HabitGoalT where
  data PrimaryKey HabitGoalT f = HabitGoalId (Columnar f UUID) (Columnar f UUID) deriving (Generic)
  primaryKey (HabitGoal hid gid) = HabitGoalId hid gid

instance Beamable (PrimaryKey HabitGoalT)

data IntentionGoalT f = IntentionGoal
  { intentionId :: Columnar f UUID,
    goalId :: Columnar f UUID
  }
  deriving (Generic)

type IntentionGoal = IntentionGoalT Identity

instance Beamable IntentionGoalT

instance Table IntentionGoalT where
  data PrimaryKey IntentionGoalT f = IntentionGoalId (Columnar f UUID) (Columnar f UUID) deriving (Generic)
  primaryKey (IntentionGoal iid gid) = IntentionGoalId iid gid

instance Beamable (PrimaryKey IntentionGoalT)

class HabitTableSelector (p :: Interval) where
  habitTable :: HITDb f -> f (TableEntity (HabitT p))

instance HabitTableSelector 'Daily where
  habitTable = habitsDaily

instance HabitTableSelector 'Weekly where
  habitTable = habitsWeekly

class IntentionTableSelector (p :: Interval) where
  intentionTable :: HITDb f -> f (TableEntity (IntentionT p))

instance IntentionTableSelector 'Daily where
  intentionTable = intentionsDaily

instance IntentionTableSelector 'Weekly where
  intentionTable = intentionsWeekly

-- * schema

-- | names and sql types, verified against the live db by 'HIT.DB.Migrate.migrate'
checkedHitDb :: CheckedDatabaseSettings Postgres HITDb
checkedHitDb =
  runMigrationSilenced $
    HITDb
      <$> createTable
        "users"
        ( User
            (field "id" Pg.text notNull)
            (field "email" Pg.text notNull)
            (field "password_hash" Pg.text notNull)
            (field "api_token" Pg.text notNull)
            (field "created_at" utcTime notNull)
            (field "modified_at" utcTime notNull)
        )
      <*> createTable
        "goals"
        ( Goal
            (field "id" Pg.uuid notNull)
            (UserId (field "user" Pg.text notNull))
            (field "name" Pg.text notNull)
            (field "description" Pg.text notNull)
            (field "number" int notNull)
            (field "color" Pg.text notNull)
            (field "start_date" date notNull)
            (field "end_date" (maybeType date))
            (field "created_at" utcTime notNull)
            (field "modified_at" utcTime notNull)
        )
      <*> habitsSchema
      <*> habitsSchema
      <*> intentionsSchema
      <*> intentionsSchema
      <*> createTable
        "goal_habits"
        ( HabitGoal
            (field "habit_id" Pg.uuid notNull)
            (field "goal_id" Pg.uuid notNull)
        )
      <*> createTable
        "goal_intentions"
        ( IntentionGoal
            (field "intention_id" Pg.uuid notNull)
            (field "goal_id" Pg.uuid notNull)
        )

hitDb :: DatabaseSettings Postgres HITDb
hitDb = unCheckDatabase checkedHitDb

-- | daily and weekly habits share one physical table
habitsSchema :: (Typeable p) => Migration Postgres (CheckedDatabaseEntity Postgres db (TableEntity (HabitT p)))
habitsSchema =
  createTable
    "habits"
    ( Habit
        (field "id" Pg.uuid notNull)
        (UserId (field "user" Pg.text notNull))
        (field "name" Pg.text notNull)
        (field "description" (maybeType Pg.text))
        (field "interval" (retype Pg.text) notNull)
        (field "sort" jsonType notNull)
        (field "rate" jsonType notNull)
        (field "deadline" jsonType notNull)
        (field "created_at" utcTime notNull)
        (field "modified_at" utcTime notNull)
    )

-- | daily and weekly intentions share one physical table
intentionsSchema :: (Typeable p) => Migration Postgres (CheckedDatabaseEntity Postgres db (TableEntity (IntentionT p)))
intentionsSchema =
  createTable
    "intentions"
    ( Intention
        (field "id" Pg.uuid notNull)
        (UserId (field "user" Pg.text notNull))
        (field "name" Pg.text notNull)
        (field "description" (maybeType Pg.text))
        (field "interval" (retype Pg.text) notNull)
        (field "rate" jsonType notNull)
        (field "deadline" jsonType notNull)
        (field "created_at" utcTime notNull)
        (field "modified_at" utcTime notNull)
    )

-- | same sql type at another haskell type
retype :: DataType be a -> DataType be b
retype (DataType t) = DataType t

utcTime :: DataType Postgres UTCTime
utcTime = retype timestamptz

jsonType :: DataType Postgres a
jsonType = DataType pgJsonType
