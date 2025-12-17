module Main where

import EncodeDecode (tests)
import Test.Tasty (defaultMain)

main :: IO ()
main = defaultMain tests
