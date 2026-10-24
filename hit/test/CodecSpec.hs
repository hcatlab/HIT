{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedRecordDot #-}

-- | rows decode back to the domain values they encode, and invariant-breaking rows are rejected
module CodecSpec (tests) where

import Arbitraries ()
import Data.Foldable (toList)
import Data.Time (addDays)
import HIT.DB.Codec (DecodeError (..), decodeGoal, decodeHabit, decodeIntention, decodeUser, encodeGoal, encodeHabit, encodeIntention, encodeUser)
import HIT.DB.DateRange (dateRange)
import HIT.DB.Rows qualified as Row
import HIT.Types
import Test.QuickCheck ((===))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

tests :: TestTree
tests =
  testGroup
    "Codec"
    [ testProperty "users round trip" $ \u ->
        decodeUser (encodeUser u) === Right u,
      testProperty "goals round trip" $ \g ->
        decodeGoal (encodeGoal g) === Right g,
      testProperty "habits round trip" $ \(SomeHabit h) ->
        decodeHabit (encodeHabit h) (toList h.goals) === Right (SomeHabit h),
      testProperty "intentions round trip" $ \(SomeIntention i) ->
        decodeIntention (encodeIntention i) (toList i.goals) === Right (SomeIntention i),
      testProperty "weekdays bitmask round trip" $ \w ->
        fromBits (toBits w) === Just w,
      testProperty "habits without goals are rejected" $ \(SomeHabit h) ->
        decodeHabit (encodeHabit h) [] === Left NoGoals,
      testProperty "weekdays iff daily" $ \(SomeHabit h) ->
        let row = encodeHabit h
            flipped = maybe (Just everyDay) (const Nothing) row.weekdays
         in decodeHabit row {Row.weekdays = flipped} (toList h.goals) === Left (ScheduleMismatch row.interval),
      testProperty "windows fit their interval" $ \(SomeIntention i) ->
        let (start, due) = windowBounds i.window
            later = addDays 1 due
         in decodeIntention (encodeIntention i) {Row.window = dateRange start (Just later)} (toList i.goals) === Left (WindowMismatch (encodeIntention i).interval start later),
      testProperty "windows are bounded" $ \(SomeIntention i) ->
        let (start, _) = windowBounds i.window
         in decodeIntention (encodeIntention i) {Row.window = dateRange start Nothing} (toList i.goals) === Left RangeMismatch,
      testProperty "doneAt iff done" $ \(SomeIntention i) ->
        let row = encodeIntention i
            flipped = maybe (Just i.createdAt) (const Nothing) row.doneAt
         in decodeIntention row {Row.doneAt = flipped} (toList i.goals) === Left DoneMismatch
    ]
