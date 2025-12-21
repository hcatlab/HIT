{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module HIT.DB.Schema where

import Database.Beam
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
    intentionsWeekly :: f (TableEntity (IntentionT 'Weekly))
  }
  deriving (Generic)

instance Database be HITDb

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

hitDb :: DatabaseSettings be HITDb
hitDb =
  defaultDbSettings
    `withDbModification` dbModification
      { users =
          setEntityName "users"
            <> modifyTableFields
              ( User
                  (fieldNamed "id")
                  (fieldNamed "email")
                  (fieldNamed "password_hash")
                  (fieldNamed "api_token")
              ),
        goals =
          setEntityName "goals"
            <> modifyTableFields
              ( Goal
                  (fieldNamed "id")
                  (UserId (fieldNamed "user"))
                  (fieldNamed "name")
                  (fieldNamed "description")
              ),
        habitsDaily =
          setEntityName "habits_daily"
            <> modifyTableFields
              ( Habit
                  (fieldNamed "id")
                  (UserId (fieldNamed "user"))
                  (fieldNamed "name")
                  (fieldNamed "description")
                  (fieldNamed "sort")
                  (fieldNamed "rate")
                  (fieldNamed "deadline")
              ),
        habitsWeekly =
          setEntityName "habits_weekly"
            <> modifyTableFields
              ( Habit
                  (fieldNamed "id")
                  (UserId (fieldNamed "user"))
                  (fieldNamed "name")
                  (fieldNamed "description")
                  (fieldNamed "sort")
                  (fieldNamed "rate")
                  (fieldNamed "deadline")
              ),
        intentionsDaily =
          setEntityName "intentions_daily"
            <> modifyTableFields
              ( Intention
                  (fieldNamed "id")
                  (UserId (fieldNamed "user"))
                  (fieldNamed "name")
                  (fieldNamed "description")
                  (fieldNamed "rate")
                  (fieldNamed "deadline")
              ),
        intentionsWeekly =
          setEntityName "intentions_weekly"
            <> modifyTableFields
              ( Intention
                  (fieldNamed "id")
                  (UserId (fieldNamed "user"))
                  (fieldNamed "name")
                  (fieldNamed "description")
                  (fieldNamed "rate")
                  (fieldNamed "deadline")
              )
      }
