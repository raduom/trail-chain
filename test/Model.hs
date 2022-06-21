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
  , validateTx
  ) where

import           Data.Bifunctor         (second)
import           Data.Either.Validation (Validation (..), eitherToValidation,
                                         validationToEither)
import           Data.Function          ((&))
import           Data.List              (find, foldl')
import           Data.Set               (Set)
import qualified Data.Set               as Set
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
  , _inputs  :: Set Input
  , _outputs :: [Output]
  , _sigs    :: [Signature]
  } deriving (Show, Eq)

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
getTx :: ValidatedChain -> TxId -> Maybe Tx
getTx (ValidatedChain chain) txId =
  case chain of
    Genesis tx ->
      if _txId tx == txId then Just tx else Nothing
    AddTx tx chain' ->
      if _txId tx == txId then Just tx else getTx (ValidatedChain chain') txId

getNewTxs :: ValidatedChain -> TxId -> [Tx]
getNewTxs (ValidatedChain chain) txId =
  case chain of
    Genesis tx ->
      if _txId tx > txId then [tx] else []
    AddTx tx chain' ->
      if _txId tx > txId then tx :  getNewTxs (ValidatedChain chain') txId
                         else getNewTxs (ValidatedChain chain') txId

-- Semantics:
--   What does it mean for a chain of txs to be well-formed?
data ValidationError =
    UnbalancedTx
  | MissingSignature
  | BadValue
  | InvalidReference
  | DoubleSpent
  | WrongTxId
  deriving (Show)

validateChain :: Chain -> Validation [ValidationError] ValidatedChain
validateChain c@(Genesis tx) =
  second (const $ ValidatedChain c) $ validateValues tx
validateChain c@(AddTx tx chain) =
  second (const $ ValidatedChain c) $
     validateBalance chain tx
  *> validateSigs chain tx
  *> validateValues tx

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
validateSigs :: Chain -> Tx -> Validation [ValidationError] ()
validateSigs chain tx =
  eitherToValidation $ do
    ins <- validationToEither $ validateInputs (chainToList chain) tx
    if all hasSignature $ snd <$> ins
       then Right ()
       else Left  [MissingSignature]
  where
    hasSignature :: Output -> Bool
    hasSignature (addr, _) = any (verifySig addr tx) (_sigs tx)

-- All outputs must be greater than 0.
validateValues :: Tx -> Validation [ValidationError] ()
validateValues tx =
  if all (\(_, Value v) -> v > 0) $ _outputs tx
     then Success ()
     else Failure [BadValue]

-- Check for valid references and no double spending
validateInputs :: [Tx] -> Tx -> Validation [ValidationError] [(Input, Output)]
validateInputs chain tx =
  eitherToValidation $ do
    -- Lookup all references, fail if any do not resolve.
    outs <- maybe (Left [InvalidReference]) Right $
              sequence $ getRefValue chain <$> Set.toList (_inputs tx)
    -- Check for double spending.
    if any (`Set.member` allInputs chain) (_inputs tx)
       then Left  [DoubleSpent]
       else Right $ zip (Set.toList $ _inputs tx) outs

-- Utility functions for working with the chain
chainToList :: Chain -> [Tx]
chainToList (Genesis tx)     = [tx]
chainToList (AddTx tx chain) = tx : chainToList chain

validatedChainToList :: ValidatedChain -> [Tx]
validatedChainToList (ValidatedChain chain) =
  chainToList chain

validateTx :: ValidatedChain -> Tx -> Validation [ValidationError] ValidatedChain
validateTx (ValidatedChain chain) tx =
  validateChain (AddTx tx chain)

listToChain :: [Tx] -> Maybe Chain
listToChain []         = Nothing
listToChain (tx :  []) = Just $ Genesis tx
listToChain (tx : txs) = AddTx tx <$> listToChain txs

allInputs :: [Tx] -> Set Input
allInputs = foldl' Set.union Set.empty . map _inputs

allOutputs :: [Tx] -> Set Output
allOutputs = Set.fromList . concatMap _outputs

allUnspentOutputRefs :: [Tx] -> Set OutputRef
allUnspentOutputRefs chain =
  allOutputRefs chain Set.\\ allInputs chain

allOutputRefs :: [Tx] -> Set OutputRef
allOutputRefs =
  Set.fromList .
                    -- add offsets/indexes for outputs, starting with 0
  concatMap (\tx -> zip [0..] (_outputs tx)
                    -- pair the current transaction id with the output offset/index
                    & map (\(ix, _output) -> (_txId tx, ix)))

getRefValue :: [Tx] -> OutputRef -> Maybe Output
getRefValue chain (txId, ix) = do
  tx <- find (\tx -> _txId tx == txId) chain
  _outputs tx `atMay` ix
