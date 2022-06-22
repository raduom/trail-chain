module Adapter
  ( Adapter(..)
  , pureAdapter
  ) where

import qualified Data.Either.Validation as V
import           Data.Functor.Identity  (Identity, runIdentity)
import           Test.Tasty.QuickCheck

import           Model                  (Chain, Tx, TxId, ValidationError (..))
import qualified Model

{- | Thoughts on testing strategy:
     A. We need to test that the basic behaviour works (for a validated tx, once added
        to the chain is always retreivable).
          - conscious decision not to test what happens if I insert the same tx twice,
            which is an impossible scenario in real life.
     B. Ledger rules:
        This is tested using carefully crafted txs, so the advantage of property based
        testing in this case is minimised (might as well use unit tests).
          - validation of the ledger rules requires us to be able to observe validation
            errors over possibly badly formed chains.
     C. A sanity rule that states that all generated model chains are valid.
-}

data Adapter m = Adapter
  { getTx         :: Chain -> TxId -> m (Maybe Tx)
  , validateChain :: Chain -> m [ValidationError]
  , runMonadic    :: m Property -> Property
  }

pureAdapter :: Adapter Identity
pureAdapter = Adapter
  { Adapter.getTx          = modelGetTx
  , Adapter.validateChain  = modelValidateChain
  , Adapter.runMonadic     = runIdentity
  }

modelValidateChain :: Monad m => Chain -> m [ValidationError]
modelValidateChain chain =
  case Model.validateChain chain of
    V.Success ()   -> pure []
    V.Failure errs -> pure errs

modelGetTx :: Monad m => Chain -> TxId -> m (Maybe Tx)
modelGetTx chain txId = pure $ Model.getTx chain txId
