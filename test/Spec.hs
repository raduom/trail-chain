module Main where

import           Data.Either.Validation  as V
import           Test.QuickCheck.Monadic
import           Test.Tasty
import           Test.Tasty.QuickCheck

import           Adapter
import           Generator               ()
import           Model

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [modelTests]

modelTests :: TestTree
modelTests = testGroup "Model tests"
  [ testProperty "Generated chains are valid" $ prop_genChainIsValid Adapter.pureAdapter
  ]

prop_genChainIsValid
  :: Monad m
  => Adapter m
  -> Chain
  -> Property
prop_genChainIsValid adapter chain =
  monadic (runMonadic adapter) $ do
    case validateChain chain of
      V.Success _ -> assert True
      V.Failure _ -> assert False

prop_badValue
  :: Monad m
  => Adapter m
  -> Chain
  -> Property
prop_badValue = undefined

prop_missingSig
  :: Monad m
  => Adapter m
  -> Chain
  -> Property
prop_missingSig = undefined

prop_unbalancedTx
  :: Monad m
  => Adapter m
  -> Chain
  -> Property
prop_unbalancedTx = undefined

prop_invalidRef
  :: Monad m
  => Adapter m
  -> Chain
  -> Property
prop_invalidRef = undefined

prop_doubleSpend
  :: Monad m
  => Adapter m
  -> Chain
  -> Property
prop_doubleSpend = undefined
