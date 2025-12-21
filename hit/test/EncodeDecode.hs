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

instance Arbitrary (Habit Daily) where
  arbitrary =
    Habit
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary

instance Arbitrary (Habit Weekly) where
  arbitrary =
    Habit
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary

instance Arbitrary (Intention Daily) where
  arbitrary =
    Intention
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary

instance Arbitrary (Intention Weekly) where
  arbitrary =
    Intention
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary

instance Arbitrary ApiToken where
  arbitrary = ApiToken <$> arbitrary

instance Arbitrary PublicUser where
  arbitrary = PublicUser <$> arbitrary <*> arbitrary

-- Test tree
tests :: TestTree
tests =
  testGroup
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
      testProperty "encodeDecode Habit Daily" (encodeDecode :: Habit Daily -> Property),
      testProperty "encodeDecode Habit Weekly" (encodeDecode :: Habit Weekly -> Property),
      testProperty "encodeDecode Intention Daily" (encodeDecode :: Intention Daily -> Property),
      testProperty "encodeDecode Intention Weekly" (encodeDecode :: Intention Weekly -> Property),
      testProperty "encodeDecode ApiToken" (encodeDecode :: ApiToken -> Property),
      testProperty "encodeDecode PublicUser" (encodeDecode :: PublicUser -> Property)
    ]
