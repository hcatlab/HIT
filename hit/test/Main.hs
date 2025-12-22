module Main where

import ApiSpec qualified (tests)
import EncodeDecode qualified (tests)
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main = do
  apiTests <- ApiSpec.tests
  let encodeDecodeTests = EncodeDecode.tests
  defaultMain
    ( testGroup
        "HIT Test Suite"
        [ testGroup "JSON Encode/Decode Tests" [encodeDecodeTests],
          testGroup "API Tests" [apiTests]
        ]
    )
