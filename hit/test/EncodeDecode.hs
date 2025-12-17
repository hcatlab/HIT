{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module EncodeDecode (tests) where

import Data.Aeson (FromJSON, ToJSON, Value (..), eitherDecode, encode)
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Lazy.UTF8 qualified as LBU
import Data.Text (Text)
import Data.Text qualified as T
import Data.UUID (UUID)
import Data.UUID qualified as UUID
import Data.Word (Word32)
import Test.QuickCheck
  ( Arbitrary (..),
    Gen,
    chooseInt,
    counterexample,
    elements,
    listOf,
    suchThat,
    (===),
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (Property, testProperty)
import Types

instance Arbitrary Fraction where
  arbitrary =
    do
      x <- arbitrary
      y <- arbitrary `suchThat` (/= 0)
      return $ Fraction x y

encodeDecode :: (Eq a, Show a, ToJSON a, FromJSON a) => a -> Property
encodeDecode x =
  case eitherDecode (encode x) of
    Right y -> y === x
    Left err -> counterexample err False

newtype FracLBS = FracLBS {getLBS :: LBS.ByteString}
  deriving (Eq, Show)

instance Arbitrary FracLBS where
  arbitrary = do
    x <- arbitrary :: Gen Int
    y <- arbitrary `suchThat` (/= 0) :: Gen Int
    return . FracLBS . LBU.fromString $ ['"'] ++ show x ++ "/" ++ show y ++ ['"']

decodeEncode :: FracLBS -> Property
decodeEncode (FracLBS x) =
  case eitherDecode x of
    Right (y :: Fraction) -> encode y === x
    Left err -> counterexample err False

-- Test tree
tests :: TestTree
tests =
  testGroup
    "Encode-Decode Tests for JSON instances"
    [ testProperty "encodeDecode Fraction" (encodeDecode :: Fraction -> Property),
      testProperty "decodeEncode Fraction" (decodeEncode :: FracLBS -> Property)
    ]
