module Main where

import ApiSpec qualified as ApiSpec (tests)
import EncodeDecode qualified as EncodeDecode (tests)
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
