module Main where

import           Data.Either.Validation  as V
import           Test.Tasty
import           Test.Tasty.QuickCheck

import           Generator ()
import           Model

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
  [ testProperty "Generated chains are valid" $ prop_genChainIsValid
  ]

prop_genChainIsValid :: Chain -> Property
prop_genChainIsValid chain = property $
  case validateChain chain of
    V.Success _ -> True
    V.Failure _ -> False
