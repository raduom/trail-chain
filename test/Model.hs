module Model
  ( Address
  , Signature
  , TxId
  , Value(..)
  , Input
  , Output
  , Tx(..)
  , Chain(..)
  , ValidationError(..)
  , ValidatedChain
  , sign
  , verifySig
  , getTx
  , getNewTxs
  , allInputs
  , allOutputs
  , allUnspentOutputRefs
  , allOutputRefs
  , getRefValue
  , validateChain
  , listToChain
  , chainToList
  , validatedChainToList
  ) where

import           Data.Bifunctor         (bimap)
import           Data.Either.Validation (Validation (..), eitherToValidation,
                                         validationToEither)
import           Data.Function          ((&), on)
import           Data.List              (find, (\\))
import           Safe                   (atMay)

-- Syntax
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
  , _inputs  :: [Input]
  , _outputs :: [Output]
  , _sigs    :: [Signature]
  } deriving (Show, Eq)

instance Ord Tx where
  compare = compare `on` _txId

data Chain = Genesis Tx
           | AddTx Tx Chain
           deriving (Show, Eq)

newtype ValidatedChain = ValidatedChain Chain
  deriving (Show, Eq)

-- Cryptography
sign :: Tx -> Address -> Signature
sign _ addr = addr <> ".signed"

verifySig :: Address -> Tx -> Signature -> Bool
verifySig address _tx signature = address <> ".signed" == signature

-- Semantics:
--   How does the syntax affect the observations made on the chain?
type ChainObs a = Validation [ValidationError] a

data ValidationError =
    UnbalancedTx
  | MissingSignature
  | BadValue
  | InvalidReference
  | DoubleSpent
  | WrongTxId
  deriving (Show)

getTx :: Chain -> TxId -> ChainObs (Maybe Tx)
getTx chain txId =
  observeValidChain chain $
    case chain of
      Genesis tx ->
        if _txId tx == txId then pure $ Just tx else pure Nothing
      AddTx tx chain' ->
        if _txId tx == txId then pure $ Just tx else getTx chain' txId

getNewTxs :: Chain -> TxId -> ChainObs [Tx]
getNewTxs chain txId =
  observeValidChain chain $
    case chain of
      Genesis tx ->
        if _txId tx > txId then pure [tx] else pure []
      AddTx tx chain' ->
        if _txId tx > txId then (tx :) <$> getNewTxs chain' txId
                           else getNewTxs chain' txId

-- Semantics:
--   What does it mean for a chain of txs to be well-formed?
validateChain :: Chain -> Validation [ValidationError] ValidatedChain
validateChain c@(Genesis tx) =
  bimap id (const $ ValidatedChain c) $ validateValues tx
validateChain c@(AddTx tx chain) =
  bimap id (const $ ValidatedChain c) $
     validateBalance chain tx
  <> validateOrdering (chainToList chain) tx
  <> validateSigs tx
  <> validateValues tx

observeValidChain :: Chain -> Validation [ValidationError] a -> ChainObs a
observeValidChain chain v =
  case validateChain chain of
    Success (ValidatedChain _) -> v
    Failure e                  -> Failure e

validateBalance :: Chain -> Tx -> Validation [ValidationError] ()
validateBalance chain tx =
  eitherToValidation $ do
    let c = chainToList chain
    ins  <- validationToEither $ validateInputs c tx
    let outs = _outputs tx
    if sum (snd . snd <$> ins) == sum (snd <$> outs)
       then Right ()
       else Left [UnbalancedTx]

-- All outputs must have corresponding signatures.
validateSigs :: Tx -> Validation [ValidationError] ()
validateSigs tx =
  if all (\(addr, _) -> any (verifySig addr tx) (_sigs tx)) $ _outputs tx
     then Success ()
     else Failure [MissingSignature]

-- All outputs must be greater than 0.
validateValues :: Tx -> Validation [ValidationError] ()
validateValues tx =
  if all (\(_, Value v) -> v > 0) $ _outputs tx
     then Success ()
     else Failure [BadValue]

validateOrdering :: [Tx] -> Tx -> Validation [ValidationError] ()
validateOrdering (tx : txs) tx' =
  if tx < tx' then Success()
              else Failure [WrongTxId]

-- Check for valid references and no double spending
validateInputs :: [Tx] -> Tx -> Validation [ValidationError] [(Input, Output)]
validateInputs chain tx =
  eitherToValidation $ do
    -- Lookup all references, fail if any do not resolve.
    outs <- maybe (Left [InvalidReference]) Right $
              sequence $ getRefValue chain <$> _inputs tx
    -- Check for double spending.
    if any (`elem` allInputs chain) (_inputs tx)
       then Left  [DoubleSpent]
       else Right $ zip (_inputs tx) outs

-- Utility functions for working with the chain
chainToList :: Chain -> [Tx]
chainToList (Genesis tx)     = [tx]
chainToList (AddTx tx chain) = tx : chainToList chain

validatedChainToList :: ValidatedChain -> [Tx]
validatedChainToList (ValidatedChain chain) =
  chainToList chain

listToChain :: [Tx] -> Maybe Chain
listToChain []         = Nothing
listToChain (tx :  []) = Just $ Genesis tx
listToChain (tx : txs) = AddTx tx <$> listToChain txs

allInputs :: [Tx] -> [Input]
allInputs = concatMap _inputs

allOutputs :: [Tx] -> [Output]
allOutputs = concatMap _outputs

allUnspentOutputRefs :: [Tx] -> [OutputRef]
allUnspentOutputRefs chain =
  allOutputRefs chain \\ allInputs chain

allOutputRefs :: [Tx] -> [OutputRef]
allOutputRefs =
                    -- add offsets/indexes for outputs, starting with 0
  concatMap (\tx -> zip [0..] (_outputs tx)
                    -- pair the current transaction id with the output offset/index
                    & map (\(ix, _output) -> (_txId tx, ix)))

getRefValue :: [Tx] -> OutputRef -> Maybe Output
getRefValue chain (txId, ix) = do
  tx <- find (\tx -> _txId tx == txId) chain
  _outputs tx `atMay` ix
