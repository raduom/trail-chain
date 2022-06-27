module FastChain
  ( Address
  , Signature
  , TxId
  , Tx(..)
  , Value(..)
  , OutputRef
  , Input
  , Output
  , Chain
  , ValidationError(..)
  , newChain
  , addTx
  , getTx
  , getTxs
  , validateTx
  ) where

import           Data.Either.Validation (Validation (..), eitherToValidation,
                                         validationToEither)
import           Data.Foldable          (foldl')
import           Data.Functor           (($>))
import           Data.List.NonEmpty     (NonEmpty ((:|)), toList, (<|))
import           Data.Map               (Map)
import qualified Data.Map               as Map
import           Data.Set               (Set)
import qualified Data.Set               as Set

-- This is cut & pasted from the mode. Generally you would have a very different
-- API for the real implementation, but for the purposes of this exercise this
-- will do.

type Address   = String
type Signature = String
type TxId      = Int
newtype Value  = Value Int
  deriving (Eq, Ord, Show, Num)

instance Semigroup Value where
  Value a <> Value b = Value $ a + b

type OutputRef = (TxId, Int)
type Input     = OutputRef
type Output    = (Address, Value)

data Tx = Tx
  { _txId    :: TxId
  , _inputs  :: Set Input
  , _outputs :: [Output]
  , _sigs    :: Set Signature
  } deriving (Show, Eq)

data Chain = Chain
  { _blocks :: NonEmpty Tx
  , _utxo   :: Map OutputRef Output
  , _txs    :: Map TxId Tx
  }

newChain :: Tx -> Validation [ValidationError] Chain
newChain tx =
  let utxo = foldl' (addUtxo tx) Map.empty (zip [0..] $ _outputs tx)
      txs  = Map.insert (_txId tx) tx Map.empty
  in  validateValues tx
      $> Chain { _blocks = tx :| []
               , _utxo   = utxo
               , _txs    = txs
               }

addUtxo :: Tx -> Map OutputRef Output -> (Int, Output) -> Map OutputRef Output
addUtxo tx acc (ix, output) = Map.insert (_txId tx, ix) output acc

-- Cryptography
sign :: Tx -> Address -> Signature
sign _ addr = addr <> ".signed"

verifySig :: Address -> Tx -> Signature -> Bool
verifySig address _tx signature = address <> ".signed" == signature

-- Add a new tx to the blockchain and update indices.
addTx :: Chain -> Tx -> Validation [ValidationError] Chain
addTx chain tx =
  eitherToValidation $ do
    validationToEither $ validateTx chain tx
    let utxo'  = foldl' removeUtxo   (_utxo chain) (_inputs tx)
        utxo'' = foldl' (addUtxo tx) utxo'         (zip [0..] $ _outputs tx)
        txs'   = Map.insert (_txId tx) tx (_txs chain)
    pure $ Chain { _blocks = tx <| (_blocks chain)
                 , _utxo   = utxo''
                 , _txs    = txs'
                 }
  where
    removeUtxo :: Map OutputRef Output -> OutputRef -> Map OutputRef Output
    removeUtxo acc output = Map.delete output acc

getTx :: Chain -> TxId -> Maybe Tx
getTx chain txId = Map.lookup txId $ _txs chain

getTxs :: Chain -> TxId -> [Tx]
getTxs (Chain txs _ _) txId =
  dropWhile (\tx -> _txId tx /= txId) $ toList txs

-- Validation

-- Note: This is mostly the same code as the model. Usually this is a code smell. It
--       means that either the model is too complicated or the implementation too
--       simple.

data ValidationError =
    UnbalancedTx
  | MissingSignature
  | BadValue
  | InvalidReference
  | DoubleSpent
  | WrongTxId
  deriving (Show, Eq)

validateTx :: Chain -> Tx -> Validation [ValidationError] ()
validateTx chain tx =
     validateBalance chain tx
  *> validateSigs chain tx
  *> validateValues tx

validateInputs :: Chain -> Tx -> Validation [ValidationError] [(Input, Output)]
validateInputs chain tx =
  eitherToValidation $ do
    -- I would have to scan all the spent utxos to find out if this is an invalid
    -- reference or a double spending attempt, which will take a lot of time, so
    -- I am not doing it in the real client.
    outs <- maybe (Left [DoubleSpent, InvalidReference]) Right $
              sequence $ (_utxo chain Map.!?) <$> Set.toList (_inputs tx)
    pure $ zip (Set.toList $ _inputs tx) outs

validateValues :: Tx -> Validation [ValidationError] ()
validateValues tx =
  if all (\(_, Value v) -> v > 0) $ _outputs tx
     then Success ()
     else Failure [BadValue]

validateSigs :: Chain -> Tx -> Validation [ValidationError] ()
validateSigs chain tx =
  eitherToValidation $ do
    ins <- validationToEither $ validateInputs chain tx
    if all hasSignature $ snd <$> ins
       then Right ()
       else Left  [MissingSignature]
  where
    hasSignature :: Output -> Bool
    hasSignature (addr, _) = any (verifySig addr tx) (_sigs tx)

validateBalance :: Chain -> Tx -> Validation [ValidationError] ()
validateBalance chain tx =
  eitherToValidation $ do
    ins  <- validationToEither $ validateInputs chain tx
    let outs = _outputs tx
    if sum (snd . snd <$> ins) == sum (snd <$> outs)
       then Right ()
       else Left [UnbalancedTx]
