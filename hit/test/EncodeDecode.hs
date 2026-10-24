{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module EncodeDecode (tests) where

import Arbitraries ()
import Data.Aeson (FromJSON, ToJSON, eitherDecode, encode)
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Lazy.UTF8 qualified as LBU
import Data.Set qualified as Set
import Data.Time (DayOfWeek (..))
import HIT.API.Goals (CreateGoalRequest (..), GoalResponse (..), UpdateGoalRequest (..))
import HIT.API.Habits (HabitRequest (..), HabitResponse (..))
import HIT.API.Intentions (CreateIntentionRequest (..), IntentionResponse (..), UpdateIntentionRequest (..))
import HIT.API.Users (PublicUser)
import HIT.Types
import HIT.Types.Refined (refine)
import Test.QuickCheck
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

encodeDecode :: (Eq a, Show a, ToJSON a, FromJSON a) => a -> Property
encodeDecode x =
  case eitherDecode (encode x) of
    Right y -> y === x
    Left err -> counterexample err False

newtype LBS a = LBS {getLBS :: LBS.ByteString} deriving (Eq, Show)

decodeEncode :: forall a. (FromJSON a, ToJSON a) => LBS a -> Property
decodeEncode (LBS lbs) =
  case eitherDecode @a lbs of
    Right y -> encode y === lbs
    Left err -> counterexample err False

instance Arbitrary (LBS Interval) where
  arbitrary = LBS . LBU.fromString <$> elements ["\"daily\"", "\"weekly\""]

-- test tree
tests :: TestTree
tests =
  testGroup
    "Property Tests"
    [ testGroup
        "Encode-Decode Tests for JSON instances"
        [ testProperty "encodeDecode Interval" (encodeDecode :: Interval -> Property),
          testProperty "decodeEncode Interval" (decodeEncode :: LBS Interval -> Property),
          testProperty "encodeDecode Weekdays" (encodeDecode :: Weekdays -> Property),
          testProperty "Weekdays as day names"
            $ once
            $ either (const Nothing) (Just . encode . Weekdays) (refine (Set.fromList [Friday, Wednesday])) === Just "[\"wednesday\",\"friday\"]",
          testProperty "encodeDecode CreateGoalRequest" (encodeDecode :: CreateGoalRequest -> Property),
          testProperty "encodeDecode UpdateGoalRequest" (encodeDecode :: UpdateGoalRequest -> Property),
          testProperty "encodeDecode GoalResponse" (encodeDecode :: GoalResponse -> Property),
          testProperty "encodeDecode HabitRequest" (encodeDecode :: HabitRequest -> Property),
          testProperty "encodeDecode HabitResponse" (encodeDecode :: HabitResponse -> Property),
          testProperty "encodeDecode CreateIntentionRequest" (encodeDecode :: CreateIntentionRequest -> Property),
          testProperty "encodeDecode UpdateIntentionRequest" (encodeDecode :: UpdateIntentionRequest -> Property),
          testProperty "encodeDecode IntentionResponse" (encodeDecode :: IntentionResponse -> Property),
          testProperty "encodeDecode APIToken" (encodeDecode :: APIToken -> Property),
          testProperty "encodeDecode PublicUser" (encodeDecode :: PublicUser -> Property)
        ]
    ]
