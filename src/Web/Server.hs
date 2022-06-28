module Web.Server
  ( server
  , handleTx
  , handleTxFrom
  , handleTxPush
  ) where

import           Control.Concurrent.STM      (atomically)
import           Control.Concurrent.STM.TVar (TVar, newTVarIO, readTVar,
                                              readTVarIO, writeTVar)
import           Control.Monad.IO.Class      (liftIO)
import           Data.Either.Validation      (Validation (..))
import           Servant

import           FastChain
import           Web.API

server :: TVar Chain -> Server ChainAPI
server chain =
  handleTx chain :<|> handleTxFrom chain :<|> handleTxPush chain

handleTx :: TVar Chain -> TxId -> Handler (Maybe Tx)
handleTx tchain txId = do
  chain <- liftIO $ readTVarIO tchain
  pure  $ getTx chain txId

handleTxFrom :: TVar Chain -> TxId -> Handler [Tx]
handleTxFrom tchain txId = do
  chain <- liftIO $ readTVarIO tchain
  pure  $ getTxs chain txId

handleTxPush :: TVar Chain -> Tx -> Handler [ValidationError]
handleTxPush tchain tx =
  liftIO . atomically $ do
    chain <- readTVar tchain
    case addTx chain tx of
      Success nextChain -> do
        writeTVar tchain nextChain
        pure []
      Failure es -> pure es
