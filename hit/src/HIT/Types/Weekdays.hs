{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeApplications #-}

module HIT.Types.Weekdays
  ( Weekdays (..),
    everyDay,
    member,
    toBits,
    fromBits,
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.Bits (setBit, testBit)
import Data.Int (Int16)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Time (DayOfWeek (..))
import Database.Beam (FromBackendRow (..))
import Database.Beam.Backend.SQL (HasSqlValueSyntax (..))
import Database.Beam.Postgres (Postgres)
import Database.Beam.Postgres.Syntax (PgValueSyntax)
import Refined qualified
import Refined.Unsafe (reallyUnsafeRefine)

-- | weekdays are the days a daily habit falls on, stored as a bitmask
newtype Weekdays = Weekdays
  { -- | non-empty by construction
    days :: Refined.Refined Refined.NonEmpty (Set DayOfWeek)
  }
  deriving stock (Show)
  deriving newtype (Eq, ToJSON, FromJSON)

-- | all seven days
everyDay :: Weekdays
everyDay = Weekdays (reallyUnsafeRefine (Set.fromList [Monday .. Sunday]))

-- | member d w iff d is one of the days of w
member :: DayOfWeek -> Weekdays -> Bool
member d w = Set.member d (Refined.unrefine w.days)

-- | bit i set iff iso weekday i + 1 is a member, as in the schema
toBits :: Weekdays -> Int16
toBits w = foldl setBit 0 (map bitOf (Set.toList (Refined.unrefine w.days)))

-- | nothing for the empty mask
fromBits :: Int16 -> Maybe Weekdays
fromBits n = either (const Nothing) (Just . Weekdays) (Refined.refine (Set.fromList [d | d <- [Monday .. Sunday], testBit n (bitOf d)]))

bitOf :: DayOfWeek -> Int
bitOf d = fromEnum d - 1

instance HasSqlValueSyntax PgValueSyntax Weekdays where
  sqlValueSyntax = sqlValueSyntax . toBits

instance FromBackendRow Postgres Weekdays where
  fromBackendRow = fromBackendRow @Postgres @Int16 >>= maybe (fail "empty weekdays") pure . fromBits
