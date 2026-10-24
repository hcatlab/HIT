{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module HIT.DB.Schema where

import Data.Int (Int16)
import Data.Text (Text)
import Data.Time (UTCTime)
import Database.Beam
import Database.Beam.Migrate
import Database.Beam.Postgres (Postgres)
import Database.Beam.Postgres.Migrate qualified as Pg
import Database.Beam.Postgres.Syntax (PgDataTypeDescr (..), PgDataTypeSyntax (..), emit, pgDataTypeJSON)
import HIT.DB.DateRange (dateRangeType)
import HIT.DB.Rows (GoalLinkT (..), GoalRowT (..), HabitRowT (..), IntentionRowT (..), PrimaryKey (UserRowId), UserRowT (..))
import HIT.Types.Identifier (Entity (..))
import HIT.Types.Interval (Interval)

data HITDb f = HITDb
  { users :: f (TableEntity UserRowT),
    goals :: f (TableEntity GoalRowT),
    habits :: f (TableEntity HabitRowT),
    intentions :: f (TableEntity IntentionRowT),
    habitGoals :: f (TableEntity (GoalLinkT 'HabitEntity)),
    intentionGoals :: f (TableEntity (GoalLinkT 'IntentionEntity))
  }
  deriving (Generic)

instance Database Postgres HITDb

-- * schema

-- | names and sql types, verified against the live db by 'HIT.DB.Migrate.migrate'
checkedHitDb :: CheckedDatabaseSettings Postgres HITDb
checkedHitDb =
  runMigrationSilenced $
    HITDb
      <$> createTable
        "users"
        ( UserRow
            (field "id" (retype Pg.text) notNull)
            (field "email" Pg.text notNull)
            (field "password_hash" Pg.text notNull)
            (field "api_token" (retype Pg.text) notNull)
            (field "timezone" Pg.text notNull)
            (field "created_at" utcTime notNull)
            (field "modified_at" utcTime notNull)
        )
      <*> createTable
        "goals"
        ( GoalRow
            (field "id" (retype Pg.uuid) notNull)
            (UserRowId (field "user" (retype Pg.text) notNull))
            (field "name" Pg.text notNull)
            (field "description" Pg.text notNull)
            (field "number" int notNull)
            (field "color" Pg.text notNull)
            (field "active" dateRangeType notNull)
            (field "created_at" utcTime notNull)
            (field "modified_at" utcTime notNull)
        )
      <*> createTable
        "habits"
        ( HabitRow
            (field "id" (retype Pg.uuid) notNull)
            (UserRowId (field "user" (retype Pg.text) notNull))
            (field "title" Pg.text notNull)
            (field "target" double notNull)
            (field "unit" (maybeType Pg.text))
            (field "interval" intervalKind notNull)
            (field "weekdays" (maybeType (retype (smallint :: DataType Postgres Int16))))
            (field "due_time" (maybeType (time Nothing)))
            (field "active" dateRangeType notNull)
            (field "created_at" utcTime notNull)
            (field "modified_at" utcTime notNull)
        )
      <*> createTable
        "intentions"
        ( IntentionRow
            (field "id" (retype Pg.uuid) notNull)
            (UserRowId (field "user" (retype Pg.text) notNull))
            (field "habit_id" (maybeType (retype Pg.uuid)))
            (field "title" Pg.text notNull)
            (field "target" double notNull)
            (field "unit" (maybeType Pg.text))
            (field "progress" double notNull)
            (field "interval" intervalKind notNull)
            (field "window" dateRangeType notNull)
            (field "due_time" (maybeType (time Nothing)))
            (field "done_at" (maybeType utcTime))
            (field "created_at" utcTime notNull)
            (field "modified_at" utcTime notNull)
        )
      <*> goalLinks "habit_goals" "habit_id"
      <*> goalLinks "intention_goals" "intention_id"

hitDb :: DatabaseSettings Postgres HITDb
hitDb = unCheckDatabase checkedHitDb

goalLinks :: (Typeable entity) => Text -> Text -> Migration Postgres (CheckedDatabaseEntity Postgres db (TableEntity (GoalLinkT entity)))
goalLinks name ownerCol =
  createTable
    name
    ( GoalLink
        (field ownerCol (retype Pg.uuid) notNull)
        (field "goal_id" (retype Pg.uuid) notNull)
    )

-- | same sql type at another haskell type
retype :: DataType be a -> DataType be b
retype (DataType t) = DataType t

-- | the interval_kind enum, matched by name
intervalKind :: DataType Postgres Interval
intervalKind = DataType (PgDataTypeSyntax (PgDataTypeDescrDomain "interval_kind") (emit "interval_kind") (pgDataTypeJSON "interval_kind"))

utcTime :: DataType Postgres UTCTime
utcTime = retype timestamptz
