{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Ethereum.RPC (
    module Ethereum.RPC.Data,
    NodeURL(..),
    SeqGen(..),
    RpcRequest(..),
    RpcError(..),
    CallResult(..),
    rpc,
    web3_clientVersion,
    web3_sha3,
    net_version,
    net_listening,
    net_peerCount,
    eth_protocolVersion,
    eth_syncing,
    eth_coinbase,
    eth_mining,
    eth_hashrate,
    eth_gasPrice,
    eth_accounts,
    eth_blockNumber,
    eth_getBalance,
    eth_getStorageAt,
    eth_getTransactionCount,
    eth_getBlockTransactionCountByHash,
    eth_getBlockTransactionCountByNumber,
    eth_getUncleCountByBlockHash,
    eth_getUncleCountByBlockNumber,
    eth_getCode,
    eth_sign,
    eth_sendTransaction,
    eth_call,
    eth_estimateGas,
    eth_getBlockByHash,
    eth_getBlockByHash',
    eth_getBlockByNumber,
    eth_getBlockByNumber',
    eth_getTransactionByHash,
    eth_getTransactionByBlockHashAndIndex,
    eth_getTransactionByBlockNumberAndIndex,
    eth_getTransactionReceipt,
    eth_getUncleByBlockHashAndIndex,
    eth_getUncleByBlockNumberAndIndex,
    eth_newFilter,
    eth_newBlockFilter,
    eth_newPendingTransactionFilter,
    eth_uninstallFilter,
    eth_getFilterChanges,
    eth_getFilterLogs,
    eth_getLogs
) where

import Ethereum.RPC.Data

import Control.Applicative ((<|>))
import Control.Concurrent.STM (TVar, atomically, modifyTVar, readTVar)
import Control.Lens (view)
import Control.Monad (mzero)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.Reader (MonadReader)
import Control.Monad.Trans (MonadIO, liftIO)
import Data.Aeson hiding (Error, Success)
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Context
import Data.Proxy
import Data.Text (Text)
import Network.HTTP.Conduit hiding (Proxy)
import Network.HTTP.Types.Method (methodPost)

type RpcC c m = (
    MonadIO m,
    MonadThrow m,
    MonadReader (Context c) m,
    HasContextLens c NodeURL NodeURL,
    HasContextLens c SeqGen SeqGen,
    HasContextLens c Manager Manager)

--
-- web3_
--

web3_clientVersion :: RpcC c m => m (CallResult Text)
web3_clientVersion = rpc "web3_clientVersion" []

web3_sha3 :: RpcC c m => Unformatted -> m (CallResult Hash)
web3_sha3 = rpc "web3_sha3" . pure . toJSON

--
-- net_
--

net_version :: RpcC c m => m (CallResult Network)
net_version = rpc "net_version" []

net_listening :: RpcC c m => m (CallResult Bool)
net_listening = rpc "net_listening" []

net_peerCount :: RpcC c m => m (CallResult Quantity)
net_peerCount = rpc "net_peerCount" []

--
-- eth_
--

eth_protocolVersion :: RpcC c m => m (CallResult Text)
eth_protocolVersion = rpc "eth_protocolVersion" []

eth_syncing :: RpcC c m => m (CallResult SyncStatus)
eth_syncing = rpc "eth_syncing" []

eth_coinbase :: RpcC c m => m (CallResult Address)
eth_coinbase = rpc "eth_coinbase" []

eth_mining :: RpcC c m => m (CallResult Bool)
eth_mining = rpc "eth_mining" []

eth_hashrate :: RpcC c m => m (CallResult Quantity)
eth_hashrate = rpc "eth_hashrate" []

eth_gasPrice :: RpcC c m => m (CallResult Quantity)
eth_gasPrice = rpc "eth_gasPrice" []

eth_accounts :: RpcC c m => m (CallResult [Address])
eth_accounts = rpc "eth_accounts" []

eth_blockNumber :: RpcC c m => m (CallResult Quantity)
eth_blockNumber = rpc "eth_blockNumber" []

eth_getBalance :: RpcC c m => Address -> BlockRef -> m (CallResult Quantity)
eth_getBalance addr block = rpc "eth_getBalance" [toJSON addr, toJSON block]

eth_getStorageAt :: RpcC c m => Address -> Quantity -> BlockRef -> m (CallResult Unformatted)
eth_getStorageAt addr offset block = rpc "eth_getStorageAt" [toJSON addr, toJSON offset, toJSON block]

eth_getTransactionCount :: RpcC c m => Address -> BlockRef -> m (CallResult Quantity)
eth_getTransactionCount addr block = rpc "eth_getTransactionCount" [toJSON addr, toJSON block]

eth_getBlockTransactionCountByHash :: RpcC c m => Hash -> m (CallResult Quantity)
eth_getBlockTransactionCountByHash hash = rpc "eth_getBlockTransactionCountByHash" [toJSON hash]

eth_getBlockTransactionCountByNumber :: RpcC c m => BlockRef -> m (CallResult Quantity)
eth_getBlockTransactionCountByNumber block = rpc "eth_getBlockTransactionCountByNumber" [toJSON block]

eth_getUncleCountByBlockHash :: RpcC c m => Hash -> m (CallResult Quantity)
eth_getUncleCountByBlockHash hash = rpc "eth_getUncleCountByBlockHash" [toJSON hash]

eth_getUncleCountByBlockNumber :: RpcC c m => BlockRef -> m (CallResult Quantity)
eth_getUncleCountByBlockNumber block = rpc "eth_getUncleCountByBlockNumber" [toJSON block]

eth_getCode :: RpcC c m => Address -> BlockRef -> m (CallResult Unformatted)
eth_getCode addr block = rpc "eth_getCode" [toJSON addr, toJSON block]

eth_sign :: RpcC c m => Address -> Unformatted -> m (CallResult Unformatted)
eth_sign addr dta = rpc "eth_sign" [toJSON addr, toJSON dta]

eth_sendTransaction :: RpcC c m => TransactionIn -> m (CallResult Hash)
eth_sendTransaction tx = rpc "eth_sendTransaction" [toJSON tx]

eth_call :: RpcC c m => TransactionIn -> m (CallResult Hash)
eth_call tx = rpc "eth_call" [toJSON tx]

eth_estimateGas :: RpcC c m => TransactionIn -> m (CallResult Quantity)
eth_estimateGas tx = rpc "eth_estimateGas" [toJSON tx]

eth_getBlockByHash :: RpcC c m => Hash -> m (CallResult (Block TransactionOut))
eth_getBlockByHash hash = rpc "eth_getBlockByHash" [toJSON hash, toJSON True]

eth_getBlockByHash' :: RpcC c m => Hash -> m (CallResult (Block Hash))
eth_getBlockByHash' hash = rpc "eth_getBlockByHash" [toJSON hash, toJSON False]

eth_getBlockByNumber :: RpcC c m => BlockRef -> m (CallResult (Block TransactionOut))
eth_getBlockByNumber block = rpc "eth_getBlockByNumber" [toJSON block, toJSON True]

eth_getBlockByNumber' :: RpcC c m => BlockRef -> m (CallResult (Block Hash))
eth_getBlockByNumber' block = rpc "eth_getBlockByNumber" [toJSON block,  toJSON False]

eth_getTransactionByHash :: RpcC c m => Hash -> m (CallResult TransactionOut)
eth_getTransactionByHash hash = rpc "eth_getTransactionByHash" [toJSON hash]

eth_getTransactionByBlockHashAndIndex :: RpcC c m => Hash -> Quantity -> m (CallResult TransactionOut)
eth_getTransactionByBlockHashAndIndex hash ix = rpc "eth_getTransactionByBlockHashAndIndex" [toJSON hash, toJSON ix]

eth_getTransactionByBlockNumberAndIndex :: RpcC c m => BlockRef -> Quantity -> m (CallResult TransactionOut)
eth_getTransactionByBlockNumberAndIndex block ix = rpc "eth_getTransactionByBlockNumberAndIndex" [toJSON block, toJSON ix]

eth_getTransactionReceipt :: RpcC c m => Hash -> m (CallResult TransactionReceipt)
eth_getTransactionReceipt hash = rpc "eth_getTransactionReceipt" [toJSON hash]

eth_getUncleByBlockHashAndIndex :: RpcC c m => Hash -> Quantity -> m (CallResult (Block TransactionOut))
eth_getUncleByBlockHashAndIndex block ix = rpc "eth_getUncleByBlockHashAndIndex" [toJSON block, toJSON ix]

eth_getUncleByBlockNumberAndIndex :: RpcC c m => BlockRef -> Quantity -> m (CallResult (Block TransactionOut))
eth_getUncleByBlockNumberAndIndex block ix = rpc "eth_getUncleByBlockNumberAndIndex" [toJSON block, toJSON ix]

eth_newFilter :: RpcC c m => Filter -> m (CallResult Quantity)
eth_newFilter f = rpc "eth_newFilter" [toJSON f]

eth_newBlockFilter :: RpcC c m => m (CallResult Quantity)
eth_newBlockFilter = rpc "eth_newBlockFilter" []

eth_newPendingTransactionFilter :: RpcC c m => m (CallResult Quantity)
eth_newPendingTransactionFilter = rpc "eth_newPendingTransactionFilter" []

eth_uninstallFilter :: RpcC c m => Quantity -> m (CallResult Bool)
eth_uninstallFilter filterID = rpc "eth_uninstallFilter" [toJSON filterID]

eth_getFilterChanges :: RpcC c m => Quantity -> m (CallResult FilterResult)
eth_getFilterChanges filterID = rpc "eth_getFilterChanges" [toJSON filterID]

eth_getFilterLogs :: RpcC c m => Quantity -> m (CallResult FilterResult)
eth_getFilterLogs filterID = rpc "eth_getFilterLogs" [toJSON filterID]

eth_getLogs :: RpcC c m => Filter -> m (CallResult FilterResult)
eth_getLogs f = rpc "eth_getLogs" [toJSON f]

--
--
--

rpc :: (RpcC c m, FromJSON a) => Text -> [Value] -> m (CallResult a)
rpc mthod params = do
    NodeURL url <- view (contextLens Proxy)
    mgr <- view (contextLens Proxy)
    rpcReq <- buildRpc mthod params
    req <- parseRequest url
    rsp <- flip httpLbs mgr req {
            method = methodPost,
            requestHeaders = [("Content-Type", "application/json")],
            requestBody = RequestBodyLBS $ encode rpcReq
        }
    let dta = responseBody rsp
    case decode dta of
        Just (RpcSuccess v) -> pure (Success v)
        Just (RpcFailure e) -> pure (Error e)
        Nothing -> pure (ResponseParseFailure dta)

buildRpc :: RpcC c m => Text -> [Value] -> m RpcRequest
buildRpc mthod params = do
    SeqGen gen <- view (contextLens Proxy)
    liftIO . atomically $ do
        v <- readTVar gen
        modifyTVar gen (+1)
        pure RpcRequest {
            reqID = v,
            reqMethod = mthod,
            reqParams = params
        }

--
-- Utility
--

newtype NodeURL = NodeURL String

newtype SeqGen = SeqGen (TVar Int)

data CallResult a
    = Success a
    | Error RpcError
    | ResponseParseFailure ByteString
    deriving Show

instance Functor CallResult where
    fmap f (Success a) = Success (f a)
    fmap _ (Error e) = Error e
    fmap _ (ResponseParseFailure s) = ResponseParseFailure s

data RpcRequest = RpcRequest {
    reqID :: Int,
    reqMethod :: Text,
    reqParams :: [Value]
}

instance ToJSON RpcRequest where
    toJSON r =
        object [
            "jsonrpc" .= ("2.0" :: Text),
            "id" .= reqID r,
            "method" .= reqMethod r,
            "params" .= reqParams r
        ]

data RpcResponse a
    = RpcSuccess a
    | RpcFailure RpcError

instance FromJSON a => FromJSON (RpcResponse a) where
    parseJSON (Object v) = (RpcSuccess <$> v .: "result") <|> (RpcFailure <$> v .: "error")
    parseJSON _ = mzero

data RpcError = RpcError {
    errCode :: Integer,
    errMessage :: Text,
    errData :: Maybe Value
} deriving Show

instance FromJSON RpcError where
    parseJSON (Object v) =
        RpcError    <$> v .: "code"
                    <*> v .: "message"
                    <*> v .:? "data"
    parseJSON _ = mzero
