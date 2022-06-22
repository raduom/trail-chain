module Main where

import           Data.Either.Validation  as V
import           Data.List               (nub)
import           Data.Maybe              (isJust, fromJust)
import qualified Data.Set                as Set
import           Test.QuickCheck.Monadic
import           Test.Tasty
import           Test.Tasty.QuickCheck

import           Adapter                 (Adapter (..))
import qualified Adapter
import           Generator
import           Model                   (Chain (..), Tx (..),
                                          ValidationError (..), Value (..),
                                          chainToList, getRefValue, sign)
import qualified Model

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [modelTests]

modelTests :: TestTree
modelTests = testGroup "Model tests"
  [ testProperty "Double spend" $ prop_doubleSpend Adapter.pureAdapter
  , testProperty "Invalid reference" $ prop_invalidRef Adapter.pureAdapter
  , testProperty "Unbalanced tx" $ prop_unbalancedTx Adapter.pureAdapter
  , testProperty "Missing signature" $ prop_missingSig Adapter.pureAdapter
  , testProperty "Negative values" $ prop_badValue Adapter.pureAdapter
  , testProperty "Generated chains are valid" $ prop_genChainIsValid Adapter.pureAdapter
  , testProperty "Can get old txs" $ prop_canFindTx Adapter.pureAdapter
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

-- Ideally, we should also test for equality here.
prop_canFindTx
  :: Monad m
  => Adapter m
  -> Chain
  -> Property
prop_canFindTx adapter chain =
  let chain' = chainToList chain
  in  forAll (chooseInt (1, length chain')) $
  \txId ->
    monadic (runMonadic adapter) $ do
      tx <- run $ Adapter.getTx adapter chain txId
      assert $ isJust tx

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
       assert $ nub result == [BadValue]

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
      assert $ nub result == [MissingSignature]

prop_unbalancedTx
  :: Monad m
  => Adapter m
  -> Chain
  -> Property
prop_unbalancedTx adapter chain =
  forAll (genTx $ chainToList chain) $
  \tx ->
    monadic (runMonadic adapter) $ do
      let out = head $ _outputs tx
          tx'    = tx { _outputs = out : _outputs tx }
      result <- run $ Adapter.validateChain adapter (AddTx tx' chain)
      assert $ nub result == [UnbalancedTx]

prop_invalidRef
  :: Monad m
  => Adapter m
  -> Chain
  -> Property
prop_invalidRef adapter chain =
  forAll (genTx $ chainToList chain) $
  \tx ->
    monadic (runMonadic adapter) $ do
      let tx' = tx { _inputs = (-1, 10) `Set.insert` _inputs tx }
      result <- run $ Adapter.validateChain adapter (AddTx tx' chain)
      assert $ nub result == [InvalidReference]

prop_doubleSpend
  :: Monad m
  => Adapter m
  -> Chain
  -> Property
prop_doubleSpend adapter chain =
  let chain' = chainToList chain in
  forAll (genTx chain') $
  \tx0 ->
  forAll (genTx $ tx0 : chain') $
  \tx1 ->
  forAll (head <$> shuffle addresses) $
  \addr ->
    monadic (runMonadic adapter) $ do
      let ref0 = Set.elemAt 0 $ _inputs tx0
          (addr0, value0) = fromJust $ getRefValue chain' ref0
          tx1' = tx1 { _inputs  = ref0 `Set.insert` _inputs tx1
                     , _outputs = (addr, value0) : _outputs tx1
                     , _sigs    = (sign tx1 addr0) `Set.insert` _sigs tx1
                     }
      -- First tx is succesfully processed.
      result0 <- run $ Adapter.validateChain adapter (AddTx tx0 chain)
      assert   $ result0 == []

      result1 <- run $ Adapter.validateChain adapter (AddTx tx1'
                                                     (AddTx tx0 chain))
      assert   $ nub result1 == [DoubleSpent]
