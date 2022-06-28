module Main where

import           Control.Concurrent.STM.TVar (newTVarIO)
import           Data.Either.Validation      (Validation (..))
import qualified Data.Set                    as Set
import           Network.Wai.Handler.Warp    (run)
import           Servant

import           FastChain
import           Web.API
import           Web.Server


initialDistribution :: [(Address, Value)]
initialDistribution =
  [ ("plot", Value 1_000_000_000), ("half", Value 1_000_000_000)
  , ("inch", Value 1_000_000_000), ("hold", Value 1_000_000_000)
  , ("easy", Value 1_000_000_000), ("calm", Value 1_000_000_000)
  , ("user", Value 1_000_000_000), ("feet", Value 1_000_000_000)
  , ("gift", Value 1_000_000_000), ("step", Value 1_000_000_000)
  ]

mkGenesisTx :: [(Address, Value)] -> Tx
mkGenesisTx distribution =
  let tx = Tx { _txId = 0
              , _inputs = Set.empty
              , _outputs = initialDistribution
              , _sigs = Set.empty
              }
  in tx { _sigs = Set.fromList $ sign tx <$> map fst distribution }

initialChain :: Chain
initialChain =
  case newChain $ mkGenesisTx initialDistribution of
    Success chain -> chain
    Failure _     -> error "This should never happen."

main :: IO ()
main = do
  tchain <- newTVarIO initialChain
  run 8081 $ serve (Proxy :: Proxy ChainAPI) $ server tchain
