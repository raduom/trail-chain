module Main where

import           Data.Either.Validation  as V
import           Test.QuickCheck.Monadic
import           Test.Tasty
import           Test.Tasty.QuickCheck

import           Adapter
import           Generator
import           Model

import qualified Debug.Trace as Debug

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [modelTests]

modelTests :: TestTree
modelTests = testGroup "Model tests"
  [ testProperty "Can detect negative values" $ prop_badValue Adapter.pureAdapter
  -- , testProperty "Generated chains are valid" $ prop_genChainIsValid Adapter.pureAdapter
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
  -> ValidatedChain
  -> Property
prop_badValue adapter chain =
  forAll (genTx $ validatedChainToList chain) $
  \tx   ->
  forAll (head <$> shuffle addresses) $
  \addr ->
     monadic (runMonadic adapter) $ do
       let outs   = Debug.trace "outputs 01" $ _outputs tx
           tx'    = tx { _outputs = outs ++ [ (addr, Value   10)
                                            , (addr, Value (-10)) ]
                       }
       case validateTx chain tx' of
         V.Failure [BadValue] -> Debug.trace "bad received"  $ assert True
         _                    -> Debug.trace "good received" $ assert False

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
