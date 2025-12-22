{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module EncodeDecode (tests) where

import Data.Aeson (FromJSON, ToJSON, eitherDecode, encode)
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Lazy.UTF8 qualified as LBU
import Data.Containers.ListUtils (nubInt)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)
import Data.Time (UTCTime)
import HIT.Api.Habits
  ( CreateHabitRequest (..),
    HabitDeadline (..),
    HabitResponse (..),
    HabitView (..),
    UpdateHabitRequest (..),
  )
import HIT.Api.Intentions
  ( CreateIntentionRequest (..),
    IntentionDeadline (..),
    IntentionResponse (..),
    IntentionView (..),
    UpdateIntentionRequest (..),
  )
import HIT.Types
import Test.QuickCheck
  ( Arbitrary (..),
    Gen,
    chooseInt,
    counterexample,
    elements,
    suchThat,
    (===),
  )
import Test.QuickCheck.Gen (oneof, vectorOf)
import Test.QuickCheck.Instances.Text ()
import Test.QuickCheck.Instances.UUID ()
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (Property, testProperty)

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

instance Arbitrary Fraction where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary `suchThat` (/= 0)
    return $ Fraction x y

instance Arbitrary (LBS Fraction) where
  arbitrary = do
    x <- arbitrary :: Gen Int
    y <- arbitrary `suchThat` (/= 0) :: Gen Int
    return . LBS . LBU.fromString $ ['"'] ++ show x ++ "/" ++ show y ++ ['"']

nonEmptyTextGen :: Gen (NonEmpty Text)
nonEmptyTextGen = do
  x <- arbitrary
  xs <- arbitrary
  pure (x :| xs)

instance Arbitrary Interval where
  arbitrary = elements [Daily, Weekly]

instance Arbitrary (LBS Interval) where
  arbitrary = do
    s <- elements ["\"daily\"", "\"weekly\""]
    return . LBS . LBU.fromString $ s

instance Arbitrary Hours where
  arbitrary = do
    hours <- chooseInt (0, 23)
    listOfHours <- vectorOf hours (chooseInt (0, 23))
    let uniqueSortedHours = nubInt listOfHours
    return $ Hours uniqueSortedHours

instance Arbitrary Weekdays where
  arbitrary =
    Weekdays
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary

instance Arbitrary (Deadline Daily) where
  arbitrary = DayHours <$> arbitrary

instance Arbitrary (Deadline Weekly) where
  arbitrary = WeekHours <$> arbitrary

instance Arbitrary Sort where
  arbitrary =
    oneof [return True, return False]
      >>= \toss ->
        if toss
          then YesNo <$> arbitrary
          else Times <$> arbitrary

-- Habits arbitrary instances for API payloads
instance Arbitrary HabitDeadline where
  arbitrary = do
    interval <- arbitrary :: Gen Interval
    case interval of
      Daily -> DailyDeadline <$> arbitrary
      Weekly -> WeeklyDeadline <$> arbitrary

-- Habit view (unified list)

instance Arbitrary UTCTime where
  arbitrary = pure (read "2023-01-01 00:00:00 UTC")

instance Arbitrary HabitView where
  arbitrary = do
    interval <- arbitrary :: Gen Interval
    deadline <- case interval of
      Daily -> DailyDeadline <$> arbitrary
      Weekly -> WeeklyDeadline <$> arbitrary
    goals <- arbitrary :: Gen [Text]
    createdAt <- arbitrary
    modifiedAt <- arbitrary
    HabitView
      <$> arbitrary -- id
      <*> pure interval
      <*> arbitrary -- name
      <*> arbitrary -- description
      <*> arbitrary -- sort
      <*> arbitrary -- rate
      <*> pure deadline
      <*> pure goals
      <*> pure createdAt
      <*> pure modifiedAt

-- Habit CRUD payloads and responses (p-parameterized)
instance Arbitrary (CreateHabitRequest 'Daily) where
  arbitrary = CreateHabitRequest <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> (arbitrary :: Gen (Deadline 'Daily)) <*> nonEmptyTextGen

instance Arbitrary (CreateHabitRequest 'Weekly) where
  arbitrary = CreateHabitRequest <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> (arbitrary :: Gen (Deadline 'Weekly)) <*> nonEmptyTextGen

instance Arbitrary (UpdateHabitRequest 'Daily) where
  arbitrary = UpdateHabitRequest <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> (arbitrary :: Gen (Deadline 'Daily)) <*> nonEmptyTextGen

instance Arbitrary (UpdateHabitRequest 'Weekly) where
  arbitrary = UpdateHabitRequest <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> (arbitrary :: Gen (Deadline 'Weekly)) <*> nonEmptyTextGen

instance Arbitrary (HabitResponse 'Daily) where
  arbitrary = HabitResponse <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> (arbitrary :: Gen (Deadline 'Daily)) <*> (arbitrary :: Gen [Text]) <*> arbitrary <*> arbitrary

instance Arbitrary (HabitResponse 'Weekly) where
  arbitrary = HabitResponse <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> (arbitrary :: Gen (Deadline 'Weekly)) <*> (arbitrary :: Gen [Text]) <*> arbitrary <*> arbitrary

-- Intentions arbitrary instances for API payloads
instance Arbitrary IntentionDeadline where
  arbitrary = do
    interval <- arbitrary :: Gen Interval
    case interval of
      Daily -> DailyIntentionDeadline <$> arbitrary
      Weekly -> WeeklyIntentionDeadline <$> arbitrary

-- Intention view (unified list)
instance Arbitrary IntentionView where
  arbitrary = do
    interval <- arbitrary :: Gen Interval
    deadline <- case interval of
      Daily -> DailyIntentionDeadline <$> arbitrary
      Weekly -> WeeklyIntentionDeadline <$> arbitrary
    goals <- arbitrary :: Gen [Text]
    createdAt <- arbitrary
    modifiedAt <- arbitrary
    IntentionView
      <$> arbitrary -- id
      <*> pure interval
      <*> arbitrary -- name
      <*> arbitrary -- description
      <*> arbitrary -- rate
      <*> pure deadline
      <*> pure goals
      <*> pure createdAt
      <*> pure modifiedAt

-- Intention CRUD payloads and responses (p-parameterized)
instance Arbitrary (CreateIntentionRequest 'Daily) where
  arbitrary = CreateIntentionRequest <$> arbitrary <*> arbitrary <*> arbitrary <*> (arbitrary :: Gen (Deadline 'Daily)) <*> nonEmptyTextGen

instance Arbitrary (CreateIntentionRequest 'Weekly) where
  arbitrary = CreateIntentionRequest <$> arbitrary <*> arbitrary <*> arbitrary <*> (arbitrary :: Gen (Deadline 'Weekly)) <*> nonEmptyTextGen

instance Arbitrary (UpdateIntentionRequest 'Daily) where
  arbitrary = UpdateIntentionRequest <$> arbitrary <*> arbitrary <*> arbitrary <*> (arbitrary :: Gen (Deadline 'Daily)) <*> nonEmptyTextGen

instance Arbitrary (UpdateIntentionRequest 'Weekly) where
  arbitrary = UpdateIntentionRequest <$> arbitrary <*> arbitrary <*> arbitrary <*> (arbitrary :: Gen (Deadline 'Weekly)) <*> nonEmptyTextGen

instance Arbitrary (IntentionResponse 'Daily) where
  arbitrary = IntentionResponse <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> (arbitrary :: Gen (Deadline 'Daily)) <*> (arbitrary :: Gen [Text]) <*> arbitrary <*> arbitrary

instance Arbitrary (IntentionResponse 'Weekly) where
  arbitrary = IntentionResponse <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> (arbitrary :: Gen (Deadline 'Weekly)) <*> (arbitrary :: Gen [Text]) <*> arbitrary <*> arbitrary

instance Arbitrary ApiToken where
  arbitrary = ApiToken <$> arbitrary

instance Arbitrary PublicUser where
  arbitrary = PublicUser <$> arbitrary <*> arbitrary

-- Test tree
tests :: TestTree
tests =
  testGroup
    "Property Tests"
    [ testGroup
        "Encode-Decode Tests for JSON instances"
        [ testProperty "encodeDecode Fraction" (encodeDecode :: Fraction -> Property),
          testProperty "decodeEncode Fraction" (decodeEncode :: LBS Fraction -> Property),
          testProperty "encodeDecode Interval" (encodeDecode :: Interval -> Property),
          testProperty "decodeEncode Interval" (decodeEncode :: LBS Interval -> Property),
          testProperty "encodeDecode Hours" (encodeDecode :: Hours -> Property),
          testProperty "encodeDecode Weekdays" (encodeDecode :: Weekdays -> Property),
          testProperty "encodeDecode Deadline Daily" (encodeDecode :: Deadline Daily -> Property),
          testProperty "encodeDecode Deadline Weekly" (encodeDecode :: Deadline Weekly -> Property),
          testProperty "encodeDecode Sort" (encodeDecode :: Sort -> Property),
          -- Habits unified list view and p-parameterized CRUD payloads
          testProperty "encodeDecode HabitView" (encodeDecode :: HabitView -> Property),
          testProperty "encodeDecode CreateHabitRequest Daily" (encodeDecode :: CreateHabitRequest 'Daily -> Property),
          testProperty "encodeDecode CreateHabitRequest Weekly" (encodeDecode :: CreateHabitRequest 'Weekly -> Property),
          testProperty "encodeDecode UpdateHabitRequest Daily" (encodeDecode :: UpdateHabitRequest 'Daily -> Property),
          testProperty "encodeDecode UpdateHabitRequest Weekly" (encodeDecode :: UpdateHabitRequest 'Weekly -> Property),
          testProperty "encodeDecode HabitResponse Daily" (encodeDecode :: HabitResponse 'Daily -> Property),
          testProperty "encodeDecode HabitResponse Weekly" (encodeDecode :: HabitResponse 'Weekly -> Property),
          -- Intentions unified list view and p-parameterized CRUD payloads
          testProperty "encodeDecode IntentionView" (encodeDecode :: IntentionView -> Property),
          testProperty "encodeDecode CreateIntentionRequest Daily" (encodeDecode :: CreateIntentionRequest 'Daily -> Property),
          testProperty "encodeDecode CreateIntentionRequest Weekly" (encodeDecode :: CreateIntentionRequest 'Weekly -> Property),
          testProperty "encodeDecode UpdateIntentionRequest Daily" (encodeDecode :: UpdateIntentionRequest 'Daily -> Property),
          testProperty "encodeDecode UpdateIntentionRequest Weekly" (encodeDecode :: UpdateIntentionRequest 'Weekly -> Property),
          testProperty "encodeDecode IntentionResponse Daily" (encodeDecode :: IntentionResponse 'Daily -> Property),
          testProperty "encodeDecode IntentionResponse Weekly" (encodeDecode :: IntentionResponse 'Weekly -> Property),
          testProperty "encodeDecode ApiToken" (encodeDecode :: ApiToken -> Property),
          testProperty "encodeDecode PublicUser" (encodeDecode :: PublicUser -> Property)
        ]
    ]
