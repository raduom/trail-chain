module Adapter
  ( Adapter(..)
  , pureAdapter
  ) where

import           Data.Functor.Identity (Identity, runIdentity)
import           Test.Tasty.QuickCheck

import           Model

data Adapter m = Adapter
  { getTx      :: ValidatedChain -> TxId -> m (Maybe Tx)
  , getNewTxs  :: ValidatedChain -> TxId -> m [Tx]
  , runMonadic :: m Property -> Property
  }

pureAdapter :: Adapter Identity
pureAdapter = Adapter
  { Adapter.getTx     = \vc tid -> pure $ Model.getTx     vc tid
  , Adapter.getNewTxs = \vc tid -> pure $ Model.getNewTxs vc tid
  , Adapter.runMonadic = runIdentity
  }
