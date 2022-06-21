module Adapter
  ( Adapter(..)
  , pureAdapter
  ) where

import           Data.Functor.Identity (Identity, runIdentity)
import           Test.Tasty.QuickCheck

import           Model

data Adapter m = Adapter
  { getTx         :: Chain -> TxId -> m (Maybe Tx)
  , validateChain :: Chain -> m [ValidationError]
  , runMonadic    :: m Property -> Property
  }

pureAdapter :: Adapter Identity
pureAdapter = Adapter
  { Adapter.getTx     = \c tid -> pure $ Model.getTx vc tid
  , Adapter.validate  = undefined
  , Adapter.runMonadic = runIdentity
  }
