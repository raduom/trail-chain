module FastSpec
  ( fastAdapter
  ) where

import           Data.Bifunctor         (second)
import           Data.Either            (fromRight)
import           Data.Either.Validation (Validation (..), validationToEither)
import           Data.Functor.Identity  (Identity (..))

import           Adapter                (Adapter (..))
import qualified FastChain              as FC
import qualified Model                  as M

fastAdapter :: Adapter Identity
fastAdapter = Adapter
  { Adapter.getTx      = fastGetTx
  , Adapter.validateTx = fastValidateTx
  , Adapter.runMonadic = runIdentity
  }

fastGetTx :: M.Chain -> M.TxId -> Identity (Maybe M.Tx)
fastGetTx chain txId =
  let fastChain = run chain
      fastTx    = FC.getTx fastChain txId
  in  pure $ toModelTx <$> fastTx

fastValidateTx :: M.Chain -> M.Tx -> Identity [M.ValidationError]
fastValidateTx chain tx =
  let fastChain = run chain
   in case FC.validateTx fastChain (fromModelTx tx) of
        Success () -> pure []
        Failure es -> pure $ toModelError <$> es

run :: M.Chain -> FC.Chain
run (M.Genesis tx) =
  fromRight (error "Error while interpreting model chain") $
    validationToEither $ FC.newChain $ fromModelTx tx
run (M.AddTx tx chain) =
  fromRight (error "Error while interpreting model chain") $
    validationToEither $ FC.addTx (run chain) (fromModelTx tx)

fromModelTx :: M.Tx -> FC.Tx
fromModelTx tx =
  FC.Tx { FC._txId    = M._txId tx
        , FC._inputs  = M._inputs tx
        , FC._outputs = second fromModelValue <$> M._outputs tx
        , FC._sigs    = M._sigs tx
        }

toModelTx :: FC.Tx -> M.Tx
toModelTx tx =
  M.Tx { M._txId    = FC._txId tx
       , M._inputs  = FC._inputs tx
       , M._outputs = second toModelValue <$> FC._outputs tx
       , M._sigs    = FC._sigs tx
       }

fromModelValue :: M.Value -> FC.Value
fromModelValue (M.Value v) = FC.Value v

toModelValue :: FC.Value -> M.Value
toModelValue (FC.Value v) = M.Value v

toModelError :: FC.ValidationError -> M.ValidationError
toModelError FC.UnbalancedTx     = M.UnbalancedTx
toModelError FC.MissingSignature = M.MissingSignature
toModelError FC.BadValue         = M.BadValue
toModelError FC.InvalidReference = M.InvalidReference
toModelError FC.DoubleSpent      = M.DoubleSpent
toModelError FC.WrongTxId        = M.WrongTxId
