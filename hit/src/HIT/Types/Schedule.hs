{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}

-- | when habits recur and when their occurrences are due
module HIT.Types.Schedule
  ( IsoWeek,
    isoWeekOf,
    isoWeekBounds,
    Schedule (..),
    scheduleInterval,
    Window (..),
    windowBounds,
    windowInterval,
  )
where

import Data.Kind (Type)
import Data.Time (Day, Year)
import Data.Time.Calendar.WeekDate (WeekOfYear, fromWeekDate, toWeekDate)
import Data.Type.Equality (TestEquality (..), (:~:) (..))
import HIT.Types.Interval (Interval (..))
import HIT.Types.Weekdays (Weekdays)

-- | an iso week is a week of a week-numbering year, built only from a day so every value is a real week
data IsoWeek = IsoWeek Year WeekOfYear
  deriving stock (Show, Eq, Ord)

-- | the iso week containing a day
isoWeekOf :: Day -> IsoWeek
isoWeekOf d = let (y, w, _) = toWeekDate d in IsoWeek y w

-- | its monday and sunday
isoWeekBounds :: IsoWeek -> (Day, Day)
isoWeekBounds (IsoWeek y w) = (fromWeekDate y w 1, fromWeekDate y w 7)

-- | a schedule is when a habit of an interval occurs
type Schedule :: Interval -> Type
data Schedule interval where
  -- | on these days of each week
  DailySchedule :: Weekdays -> Schedule 'Daily
  -- | once each iso week
  WeeklySchedule :: Schedule 'Weekly

deriving stock instance Show (Schedule interval)

deriving stock instance Eq (Schedule interval)

instance TestEquality Schedule where
  testEquality (DailySchedule _) (DailySchedule _) = Just Refl
  testEquality WeeklySchedule WeeklySchedule = Just Refl
  testEquality _ _ = Nothing

-- | the interval a schedule is indexed by
scheduleInterval :: Schedule interval -> Interval
scheduleInterval (DailySchedule _) = Daily
scheduleInterval WeeklySchedule = Weekly

-- | a window is the days an intention of an interval is due on
type Window :: Interval -> Type
data Window interval where
  -- | one day
  DailyWindow :: Day -> Window 'Daily
  -- | one iso week
  WeeklyWindow :: IsoWeek -> Window 'Weekly

deriving stock instance Show (Window interval)

deriving stock instance Eq (Window interval)

instance TestEquality Window where
  testEquality (DailyWindow _) (DailyWindow _) = Just Refl
  testEquality (WeeklyWindow _) (WeeklyWindow _) = Just Refl
  testEquality _ _ = Nothing

-- | its first and last day
windowBounds :: Window interval -> (Day, Day)
windowBounds (DailyWindow d) = (d, d)
windowBounds (WeeklyWindow w) = isoWeekBounds w

-- | the interval a window is indexed by
windowInterval :: Window interval -> Interval
windowInterval (DailyWindow _) = Daily
windowInterval (WeeklyWindow _) = Weekly
