module Web.API
  ( ChainAPI
  ) where

import Servant
import FastChain

type ChainAPI = "tx"      :> Capture "txid" TxId :> Get  '[JSON] (Maybe Tx)
           :<|> "tx-from" :> Capture "txid" TxId :> Get  '[JSON] [Tx]
           :<|> "tx-push" :> ReqBody '[JSON] Tx  :> Post '[JSON] [ValidationError]
