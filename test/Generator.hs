{-# OPTIONS_GHC -fno-warn-orphans #-}

module Generator
  ( genTx
  , genChain
  , addresses
  ) where

import           Control.Monad          (forM)
import           Data.Either.Validation (Validation (..))
import           Data.Maybe             (fromJust)
import           Test.QuickCheck        (Arbitrary(..), Gen, chooseInt, shuffle)

import           Model

addresses :: [Address]
addresses =
  [ "plot", "feet", "tree", "star", "jury", "plus", "slip", "pack", "meet", "else"
  , "half", "gift", "poll", "city", "stay", "nick", "said", "hour", "gate", "cost"
  , "inch", "step", "wall", "spot", "sake", "shut", "cool", "copy", "game", "work"
  , "hold", "fire", "risk", "jump", "roof", "send", "soft", "lack", "cook", "task"
  , "easy", "went", "ford", "lift", "made", "pipe", "pink", "port", "true", "nice"
  , "calm", "vary", "data", "twin", "fact", "till", "trip", "five", "jack", "well"
  , "user", "soon", "done", "save" ]

instance Arbitrary Chain where
  arbitrary = fromJust . listToChain <$> genChain

-- Assertion on the validity of the chain.
withValidatedChain :: [Tx] -> (ValidatedChain -> a) -> a
withValidatedChain txs f =
  let c = fromJust $ listToChain txs
  in  case validateChain c of
        Success vc -> f vc
        -- It is fine to use impure exceptions in test code
        Failure e  -> error $ "Invalid chain: " <> show e

genTx :: [Tx] -> Gen Tx
genTx txs = withValidatedChain txs $ \_ -> do
  utxos  <- shuffle $ allUnspentOutputRefs txs
  inCnt  <- chooseInt (1, 8)
  let inputs     = lookupInputs txs $ take inCnt utxos
      txId       = _txId (head txs) + 1
      totalValue = sum (snd . snd <$> inputs)
  outCnt <- chooseInt (1, 8)
  addrs  <- take outCnt <$> shuffle addresses
  outs   <- genOutputs totalValue addrs
  let tx = Tx { _txId    = txId
              , _inputs  = fst <$> inputs
              , _outputs = outs
              , _sigs    = []
              }
  pure $ tx { _sigs = sign tx <$> map (fst . snd) inputs }
  where
    lookupInputs :: [Tx] -> [Input] -> [(Input, Output)]
    lookupInputs c ins =
      zip ins $
        fromJust . getRefValue c <$> ins

genOutputs :: Value -> [Address] -> Gen [Output]
genOutputs (Value 0)  _       = pure []
genOutputs (Value _) []       = pure []
genOutputs (Value v) [a]      = pure [(a, Value v)]
genOutputs (Value v) (a : as) = do
  v' <- chooseInt (1, v)
  ((a, Value v') :) <$> genOutputs (Value $ v - v') as

genGenesis :: Gen Tx
genGenesis = do
  outCnt <- chooseInt (1, 16)
  addrs  <- take outCnt <$> shuffle addresses
  outs   <-
    forM addrs $ \addr -> do
      value <- chooseInt (1_000_000, 10_000_000_000)
      pure (addr, Value value)
  pure Tx { _txId = 0
          , _inputs = []
          , _outputs = outs
          , _sigs = []
          }

genChain :: Gen [Tx]
genChain =
  chooseInt (1, 1000) >>= go []
  where
    go :: [Tx] -> Int -> Gen [Tx]
    go [] n = do
      tx0 <- genGenesis
      go [tx0] n
    go txs n
      | n <= 0    = pure txs
      | otherwise = do
        tx <- genTx txs
        go (tx : txs) (n - 1)
