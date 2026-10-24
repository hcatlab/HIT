module Main where

import APISpec qualified (tests)
import CodecSpec qualified (tests)
import EncodeDecode qualified (tests)
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main = do
  apiTests <- APISpec.tests
  let encodeDecodeTests = EncodeDecode.tests
  defaultMain
    ( testGroup
        "HIT Test Suite"
        [ testGroup "JSON Encode/Decode Tests" [encodeDecodeTests],
          testGroup "Codec Tests" [CodecSpec.tests],
          testGroup "API Tests" [apiTests]
        ]
    )
