{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | arbitrary values of api payloads and domain types, shared by the property tests
module Arbitraries () where

import Data.Maybe (mapMaybe)
import Data.Time (Day (ModifiedJulianDay), DayOfWeek (..), TimeOfDay (..), UTCTime)
import HIT.API.Goals (CreateGoalRequest, GoalResponse, UpdateGoalRequest)
import HIT.API.Habits (HabitRequest, HabitResponse)
import HIT.API.Intentions (CreateIntentionRequest, IntentionResponse, UpdateIntentionRequest)
import HIT.API.Users (PublicUser)
import HIT.Types
import HIT.Types.Refined (Predicate, Refined, refine, unrefine)
import Test.QuickCheck
import Test.QuickCheck.Arbitrary.Generic (GenericArbitrary (..))
import Test.QuickCheck.Instances.Semigroup ()
import Test.QuickCheck.Instances.Text ()
import Test.QuickCheck.Instances.UUID ()

-- | refined values, growing the size until the predicate holds
instance (Arbitrary a, Predicate predicate a) => Arbitrary (Refined predicate a) where
  arbitrary = arbitrary `suchThatMap` (either (const Nothing) Just . refine)
  shrink = mapMaybe (either (const Nothing) Just . refine) . shrink . unrefine

deriving newtype instance Arbitrary (Identifier entity)

deriving newtype instance Arbitrary Username

deriving newtype instance Arbitrary APIToken

deriving newtype instance Arbitrary Weekdays

instance Arbitrary DayOfWeek where
  arbitrary = elements [Monday .. Sunday]

instance Arbitrary Interval where
  arbitrary = elements [Daily, Weekly]

instance Arbitrary TimeOfDay where
  arbitrary = TimeOfDay <$> chooseInt (0, 23) <*> chooseInt (0, 59) <*> (fromIntegral <$> chooseInt (0, 59))

instance Arbitrary UTCTime where
  arbitrary = pure (read "2023-01-01 00:00:00 UTC")

instance Arbitrary Day where
  arbitrary = ModifiedJulianDay <$> arbitrary

deriving via (GenericArbitrary CreateGoalRequest) instance Arbitrary CreateGoalRequest

deriving via (GenericArbitrary UpdateGoalRequest) instance Arbitrary UpdateGoalRequest

deriving via (GenericArbitrary GoalResponse) instance Arbitrary GoalResponse

deriving via (GenericArbitrary HabitRequest) instance Arbitrary HabitRequest

deriving via (GenericArbitrary HabitResponse) instance Arbitrary HabitResponse

deriving via (GenericArbitrary CreateIntentionRequest) instance Arbitrary CreateIntentionRequest

deriving via (GenericArbitrary UpdateIntentionRequest) instance Arbitrary UpdateIntentionRequest

deriving via (GenericArbitrary IntentionResponse) instance Arbitrary IntentionResponse

deriving via (GenericArbitrary PublicUser) instance Arbitrary PublicUser

deriving via (GenericArbitrary User) instance Arbitrary User

deriving via (GenericArbitrary Goal) instance Arbitrary Goal

-- domain values, by index
habitWith :: Schedule interval -> Gen (Habit interval)
habitWith s =
  Habit <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> pure s <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary SomeHabit where
  arbitrary = oneof [SomeHabit <$> (arbitrary >>= habitWith . DailySchedule), SomeHabit <$> habitWith WeeklySchedule]

-- | doneAt kept nothing iff not done
intentionWith :: Source origin -> Window interval -> Gen (Intention origin interval)
intentionWith s w = do
  i <- Intention <$> arbitrary <*> arbitrary <*> pure s <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> pure w <*> arbitrary <*> pure Nothing <*> arbitrary <*> arbitrary <*> arbitrary
  now <- arbitrary
  pure (recordProgress now i.progress i)

instance Arbitrary SomeIntention where
  arbitrary = do
    day <- arbitrary
    habit <- arbitrary
    oneof
      [ SomeIntention <$> intentionWith OneOffSource (DailyWindow day),
        SomeIntention <$> intentionWith OneOffSource (WeeklyWindow (isoWeekOf day)),
        SomeIntention <$> intentionWith (RecurringSource habit) (DailyWindow day),
        SomeIntention <$> intentionWith (RecurringSource habit) (WeeklyWindow (isoWeekOf day))
      ]
