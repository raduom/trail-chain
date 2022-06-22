module Main where

import           Data.Either.Validation  as V
import qualified Data.Set as Set
import           Test.QuickCheck.Monadic
import           Test.Tasty
import           Test.Tasty.QuickCheck

import           Adapter                 (Adapter (..))
import qualified Adapter
import           Generator
import           Model                   (Chain (..), Tx (..),
                                          ValidationError (..), Value (..),
                                          chainToList)
import qualified Model

import qualified Debug.Trace as Debug

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [modelTests]

modelTests :: TestTree
modelTests = testGroup "Model tests"
  [ testProperty "Can detect a missing signature" $ prop_missingSig Adapter.pureAdapter
  -- , testProperty "Can detect negative values"     $ prop_badValue Adapter.pureAdapter
  -- , testProperty "Generated chains are valid"     $ prop_genChainIsValid Adapter.pureAdapter
  ]

prop_genChainIsValid
  :: Monad m
  => Adapter m
  -> Chain
  -> Property
prop_genChainIsValid adapter chain =
  monadic (runMonadic adapter) $ do
    case Model.validateChain chain of
      V.Success _ -> assert True
      V.Failure _ -> assert False

prop_badValue
  :: Monad m
  => Adapter m
  -> Chain
  -> Property
prop_badValue adapter chain =
  forAll (genTx $ chainToList chain) $
  \tx   ->
  forAll (head <$> shuffle addresses) $
  \addr ->
     monadic (runMonadic adapter) $ do
       let outs   = _outputs tx
           tx'    = tx { _outputs = outs ++ [ (addr, Value   10)
                                            , (addr, Value (-10)) ]
                       }
       result <- run $ Adapter.validateChain adapter (AddTx tx' chain)
       case result of
         [BadValue] -> assert True
         _          -> assert False

prop_missingSig
  :: Monad m
  => Adapter m
  -> Chain
  -> Property
prop_missingSig adapter chain =
  forAll (genTx $ chainToList chain) $
  \tx ->
    monadic (runMonadic adapter) $ do
      let tx' = tx { _sigs = Set.drop 1 $ _sigs tx }
      result <- run $ Adapter.validateChain adapter (AddTx tx' chain)
      case result of
        [MissingSignature] -> assert True
        e                  -> Debug.trace ("Failed with: " <> show e) $ assert False

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
