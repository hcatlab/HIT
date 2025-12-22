{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module HIT.DB.Schema where

import Data.UUID (UUID)
import Database.Beam
import Database.Beam.Postgres (Postgres)
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

-- Mapping tables between goals and habits/intentions
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

hitDb :: DatabaseSettings Postgres HITDb
hitDb =
  defaultDbSettings
    `withDbModification` HITDb
      { users =
          setEntityName "users"
            <> modifyTableFields
              ( User
                  (fieldNamed "id")
                  (fieldNamed "email")
                  (fieldNamed "password_hash")
                  (fieldNamed "api_token")
                  (fieldNamed "created_at")
                  (fieldNamed "modified_at")
              ),
        goals =
          setEntityName "goals"
            <> modifyTableFields
              ( Goal
                  (fieldNamed "id")
                  (UserId (fieldNamed "user"))
                  (fieldNamed "name")
                  (fieldNamed "description")
                  (fieldNamed "created_at")
                  (fieldNamed "modified_at")
              ),
        habitsDaily = habitTableSettings,
        habitsWeekly = habitTableSettings,
        intentionsDaily = intentionTableSettings,
        intentionsWeekly = intentionTableSettings,
        habitGoals =
          setEntityName "goal_habits"
            <> modifyTableFields
              ( HabitGoal
                  (fieldNamed "habit_id")
                  (fieldNamed "goal_id")
              ),
        intentionGoals =
          setEntityName "goal_intentions"
            <> modifyTableFields
              ( IntentionGoal
                  (fieldNamed "intention_id")
                  (fieldNamed "goal_id")
              )
      }

-- Shared table configuration for daily/weekly habits stored in one physical table
habitTableSettings :: EntityModification (DatabaseEntity be db) be (TableEntity (HabitT p))
habitTableSettings =
  setEntityName "habits"
    <> modifyTableFields
      ( Habit
          (fieldNamed "id")
          (UserId (fieldNamed "user"))
          (fieldNamed "name")
          (fieldNamed "description")
          (fieldNamed "interval")
          (fieldNamed "sort")
          (fieldNamed "rate")
          (fieldNamed "deadline")
          (fieldNamed "created_at")
          (fieldNamed "modified_at")
      )

-- Shared table configuration for daily/weekly intentions stored in one physical table
intentionTableSettings :: EntityModification (DatabaseEntity be db) be (TableEntity (IntentionT p))
intentionTableSettings =
  setEntityName "intentions"
    <> modifyTableFields
      ( Intention
          (fieldNamed "id")
          (UserId (fieldNamed "user"))
          (fieldNamed "name")
          (fieldNamed "description")
          (fieldNamed "interval")
          (fieldNamed "rate")
          (fieldNamed "deadline")
          (fieldNamed "created_at")
          (fieldNamed "modified_at")
      )
