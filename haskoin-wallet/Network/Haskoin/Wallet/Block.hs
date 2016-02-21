module Network.Haskoin.Wallet.Block where

import Control.Exception (throw)
import Control.Monad.Catch (MonadThrow, throwM)
import Data.Maybe (fromMaybe)

import Network.Haskoin.Block
import Network.Haskoin.Node.HeaderTree
import Network.Haskoin.Wallet.Model
import Network.Haskoin.Wallet.Types

mainChain :: (MonadThrow m, HeaderTree m)
          => BlockHash -> BlockHash -> m [BlockHeaderNode]
mainChain bestHash blockHash = do
    bestNode <- getBlockHeaderNode bestHash >>=
        maybe (throwM $ WalletException "Best block not found") return
    blockNode <- getBlockHeaderNode blockHash >>=
        maybe (throwM $ WalletException "Block not found") return
    (_, nodes, _) <- findSplitNode bestNode blockNode
    return nodes

blockTxs :: [BlockHeaderNode] -> [WalletTx] -> [JsonBlock]
blockTxs blocks transactions = reverse $ go [] blocks transactions
  where
    go bs [] _ = bs
    go bs (n : ns) [] = go (toJsonBlock n [] : bs) ns []
    go [] (n : ns) xs = go [toJsonBlock n []] ns xs
    go (b : bs) (n : ns) (x : xs)
       | jsonBlockHash b == blockHashOf x = go
           (b{ jsonBlockTxs = toJsonTx x Nothing : jsonBlockTxs b } : bs)
           (n : ns)
           xs
       | nodeBlockHash n == blockHashOf x = go
           (toJsonBlock n [toJsonTx x Nothing] : b : bs)
           ns
           xs
       | otherwise = go
           (toJsonBlock n [] : b : bs)
           ns
           (x : xs)
    blockHashOf t = fromMaybe
        (throw $ WalletException "Unexpected unconfirmed transaction")
        (walletTxConfirmedBy t)
    toJsonBlock BlockHeaderNode{..} ts = JsonBlock
        { jsonBlockHash   = nodeBlockHash
        , jsonBlockHeight = nodeHeaderHeight
        , jsonBlockPrev   = prevBlock nodeHeader
        , jsonBlockTxs    = ts
        }

