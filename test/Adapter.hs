module Adapter
  ( Adapter(..)
  , pureAdapter
  ) where

import Model

data Adapter = Adapter
  { getTx    :: ValidatedChain -> Maybe Tx
  , getNewTx :: ValidatedChain -> TxId -> [Tx]
  }

pureAdapter :: Adapter
pureAdapter = undefined
