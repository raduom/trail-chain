module Web.Client
  ( getTx
  , getTxs
  , pushTx
  ) where

import Servant.Client
import Servant

import Web.API
import FastChain hiding (getTx, getTxs)

getTx   :: TxId -> ClientM (Maybe Tx)
getTxs  :: TxId -> ClientM [Tx]
pushTx  :: Tx   -> ClientM [ValidationError]
getTx :<|> getTxs :<|> pushTx = client (Proxy :: Proxy ChainAPI)
