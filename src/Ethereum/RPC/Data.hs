{-# LANGUAGE OverloadedStrings #-}

module Ethereum.RPC.Data (
    EthHex(..),
    Marshal(..),
    Quantity(..),
    Unformatted(..),
    Address,
    unAddress,
    mkAddress,
    Hash,
    unHash,
    mkHash,
    Event(..),
    Network(..),
    SyncStatus(..),
    SyncState(..),
    TransactionIn(..),
    TransactionOut(..),
    Call(..),
    Filter(..),
    BlockTag(..),
    BlockRef(..),
    Block(..)
) where

import Prelude hiding (take)

import Control.Applicative ((<|>))
import Control.Monad (mzero)
import Data.Aeson
import Data.Attoparsec.ByteString
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString.Char8 as B
import Data.Monoid (Monoid, (<>))
import qualified Data.Text as T
import Numeric (readHex, showHex)

class EthHex a where
    fromEthHexString :: T.Text -> Maybe a

class Marshal a where
    fromUnformatted :: Parser a
    toUnformatted :: a -> Unformatted

--
-- Quantity
--

newtype Quantity
    = Quantity { unQuantity :: Integer }
    deriving (Eq, Ord, Show)

instance ToJSON Quantity where
    toJSON (Quantity q) = String ("0x" <> hex)
        where hex = (T.toLower . T.pack) (showHex q "")

instance FromJSON Quantity where
    parseJSON (String s) | T.isPrefixOf "0x" s = do
        let encoded = drop 2 (T.unpack s)
        case readHex encoded of
            [(q, "")] -> pure (Quantity q)
            _ -> mzero
    parseJSON _ = mzero

--
-- Unformatted
--

newtype Unformatted
    = Unformatted { unUnformatted :: B.ByteString }
    deriving (Eq)

instance Monoid Unformatted where
    mempty = Unformatted ""
    mappend (Unformatted v1) (Unformatted v2) = Unformatted (v1 <> v2)

instance Show Unformatted where
    show (Unformatted v) = "Unformatted { unUnformatted = " <> show ("0x" <> B16.encode v) <> " }"

instance ToJSON Unformatted where
    toJSON (Unformatted v) = String ("0x" <> hex)
        where hex = (T.pack . B.unpack) (B16.encode v)

instance FromJSON Unformatted where
    parseJSON (String s) | T.isPrefixOf "0x" s = do
        let encoded = (B.pack . drop 2) (T.unpack s)
        case B16.decode encoded of
            (v, "") -> pure (Unformatted v)
            _ -> mzero
    parseJSON _ = mzero

instance EthHex Unformatted where
    fromEthHexString = decode . BL.pack . show

--
-- Address
--

newtype Address
    = Address { unAddress :: B.ByteString }
    deriving Eq

instance Show Address where
    show (Address a) = "Address { unAddress = " <> show ("0x" <> B16.encode a) <> " }"

instance Marshal Address where
    fromUnformatted = Address <$> take 20
    toUnformatted = Unformatted . unAddress

instance ToJSON Address where
    toJSON = toJSON . toUnformatted

instance FromJSON Address where
    parseJSON v = do
        uf <- parseJSON v
        case parse fromUnformatted (unUnformatted uf) of
            Done "" h -> pure h
            _ -> mzero

instance EthHex Address where
    fromEthHexString = decode . BL.pack . show

mkAddress :: B.ByteString -> Maybe Address
mkAddress s | B.length s == 20 = Just (Address s)
mkAddress _ = Nothing

--
-- Hash
--

newtype Hash
    = Hash { unHash :: B.ByteString }
    deriving Eq

instance Show Hash where
    show (Hash h) = "Hash { unHash = " <> show ("0x" <> B16.encode h) <> " }"

instance Marshal Hash where
    fromUnformatted = Hash <$> take 32
    toUnformatted = Unformatted . unHash

instance ToJSON Hash where
    toJSON = toJSON . toUnformatted

instance FromJSON Hash where
    parseJSON v = do
        uf <- parseJSON v
        case parse fromUnformatted (unUnformatted uf) of
            Done "" h -> pure h
            _ -> mzero

instance EthHex Hash where
    fromEthHexString = decode . BL.pack . show

mkHash :: B.ByteString -> Maybe Hash
mkHash s | B.length s == 32 = Just (Hash s)
mkHash _ = Nothing

--
-- Event
--

data Event = Event {
    eventRemoved :: Maybe Bool,
    eventLogIndex :: Maybe Quantity,
    eventTransactionIndex :: Maybe Quantity,
    eventTransactionHash :: Maybe Hash,
    eventBlockHash :: Maybe Hash,
    eventBlockNumber :: Maybe Quantity,
    eventAddress :: Address,
    eventData :: Unformatted,
    eventTopics :: [Unformatted]
} deriving (Eq, Show)

instance FromJSON Event where
    parseJSON (Object v) = do
        removed <- v .:? "removed"
        logIndex <- v .:? "logIndex"
        transactionIndex <- v .:? "transactionIndex"
        transactionHash <- v .:? "transactionHash"
        blockHash' <- v .:? "blockHash"
        blockNumber' <- v .:? "blockNumber"
        address <- v .: "address"
        dta <- v .: "data"
        topics <- v .: "topics"
        pure Event {
            eventRemoved = removed,
            eventLogIndex = logIndex,
            eventTransactionIndex = transactionIndex,
            eventTransactionHash = transactionHash,
            eventBlockHash = blockHash',
            eventBlockNumber = blockNumber',
            eventAddress = address,
            eventData = dta,
            eventTopics = topics
        }
    parseJSON _ = mzero

--
-- Network
--

data Network
    = MainNet
    | Morden
    | Ropsten
    | Rinkeby
    | Kovan
    deriving (Eq, Show)

instance FromJSON Network where
    parseJSON (String "1") = pure MainNet
    parseJSON (String "2") = pure Morden
    parseJSON (String "3") = pure Ropsten
    parseJSON (String "4") = pure Rinkeby
    parseJSON (String "42") = pure Kovan
    parseJSON _ = mzero

--
-- SyncStatus
--

data SyncStatus
    = NotSyncing
    | Syncing SyncState
    deriving (Eq, Show)

instance FromJSON SyncStatus where
    parseJSON (Bool False) = pure NotSyncing
    parseJSON o = Syncing <$> parseJSON o

data SyncState = SyncState {
    syncStartingBlock :: Quantity,
    syncCurrentBlock :: Quantity,
    syncHighestBlock :: Quantity
} deriving (Eq, Show)

instance FromJSON SyncState where
    parseJSON (Object v) =
        SyncState   <$> v .: "startingBlock"
                    <*> v .: "currentBlock"
                    <*> v .: "highestBlock"
    parseJSON _ = mzero

--
-- TransactionIn
--

data TransactionIn = TransactionIn {
    transInFrom :: Address,
    transInTo :: Maybe Address,
    transInGas :: Maybe Quantity,
    transInGasPrice :: Maybe Quantity,
    transInData :: Unformatted,
    transInNonce :: Maybe Quantity
} deriving (Eq, Show)

instance ToJSON TransactionIn where
    toJSON t =
        object [
            "from" .= transInFrom t,
            "to" .= transInTo t,
            "gas" .= transInGas t,
            "gasPrice" .= transInGasPrice t,
            "data" .= transInData t,
            "nonce" .= transInNonce t
        ]

instance FromJSON TransactionIn where
    parseJSON (Object v) = do
        from <- v .: "from"
        to <- v .:? "to"
        gas <- v .:? "gas"
        gasPrice <- v .:? "gasPrice"
        dta <- v .: "data"
        nonce <- v .:? "nonce"
        pure TransactionIn {
            transInFrom = from,
            transInTo = to,
            transInGas = gas,
            transInGasPrice = gasPrice,
            transInData = dta,
            transInNonce = nonce
        }
    parseJSON _ = mzero

--
-- TransactionOut
--

data TransactionOut = TransactionOut {
    transOutFrom :: Address,
    transOutTo :: Maybe Address,
    transOutGas :: Maybe Quantity,
    transOutGasPrice :: Maybe Quantity,
    transOutInput :: Unformatted,
    transOutNonce :: Maybe Quantity
} deriving (Eq, Show)

instance ToJSON TransactionOut where
    toJSON t =
        object [
            "from" .= transOutFrom t,
            "to" .= transOutTo t,
            "gas" .= transOutGas t,
            "gasPrice" .= transOutGasPrice t,
            "input" .= transOutInput t,
            "nonce" .= transOutNonce t
        ]

instance FromJSON TransactionOut where
    parseJSON (Object v) = do
        from <- v .: "from"
        to <- v .:? "to"
        gas <- v .:? "gas"
        gasPrice <- v .:? "gasPrice"
        input <- v .: "input"
        nonce <- v .:? "nonce"
        pure TransactionOut {
            transOutFrom = from,
            transOutTo = to,
            transOutGas = gas,
            transOutGasPrice = gasPrice,
            transOutInput = input,
            transOutNonce = nonce
        }
    parseJSON _ = mzero

--
-- Call
--

data Call = Call {
    callFrom :: Maybe Address,
    callTo :: Address,
    callGas :: Maybe Quantity,
    callGasPrice :: Maybe Quantity,
    callValue :: Maybe Quantity,
    callData :: Maybe Unformatted
} deriving (Eq, Show)

instance ToJSON Call where
    toJSON c =
        object [
            "from" .= callFrom c,
            "to" .= callTo c,
            "gas" .= callGas c,
            "gasPrice" .= callGasPrice c,
            "value" .= callValue c,
            "data" .= callData c
        ]

--
-- Filter
--

data Filter = Filter {
    filterFromBlock :: Maybe (Either Quantity BlockTag),
    filterToBlock :: Maybe (Either Quantity BlockTag),
    filterAddress :: Maybe (Either Address [Address]),
    filterTopics :: Maybe [Unformatted]
} deriving (Eq, Show)

instance ToJSON Filter where
    toJSON f = object ["from" .= from, "to" .= to, "address" .= addr, "topics" .= topics]
        where
        from = encodeEither <$> filterFromBlock f
        to = encodeEither <$> filterToBlock f
        addr = encodeEither <$> filterAddress f
        topics = filterTopics f

--
-- BlockTag
--

data BlockTag
    = Earliest
    | Latest
    | Pending
    deriving (Eq, Show)

instance ToJSON BlockTag where
    toJSON Earliest = String "earliest"
    toJSON Latest = String "latest"
    toJSON Pending = String "pending"

instance FromJSON BlockTag where
    parseJSON (String "earliest") = pure Earliest
    parseJSON (String "latest") = pure Latest
    parseJSON (String "pending") = pure Pending
    parseJSON _ = mzero

--
-- BlockRef
--

data BlockRef
    = BlockNum Quantity
    | BlockTag BlockTag
    deriving (Eq, Show)

instance ToJSON BlockRef where
    toJSON (BlockNum q) = toJSON q
    toJSON (BlockTag t) = toJSON t

instance FromJSON BlockRef where
    parseJSON v@(String _) = (BlockTag <$> parseJSON v) <|> (BlockNum <$> parseJSON v)
    parseJSON _ = mzero

--
-- Block
--

data Block a = Block {
    blockNumber :: Maybe Quantity,
    blockHash :: Maybe Hash,
    blockParentHash :: Hash,
    blockNonce :: Maybe Unformatted,
    blockSha3Uncles :: Unformatted,
    blockLogsBloom :: Maybe Unformatted,
    blockTransactionsRoot :: Unformatted,
    blockStateRoot :: Unformatted,
    blockReceiptsRoot :: Unformatted,
    blockMiner :: Address,
    blockDifficulty :: Quantity,
    blockTotalDifficulty :: Quantity,
    blockExtraData :: Unformatted,
    blockSize :: Quantity,
    blockGasLimit :: Quantity,
    blockGasUsed :: Quantity,
    blockTimestamp :: Quantity,
    blockTransactions :: [a],
    blockUncles :: [Hash]
} deriving (Eq, Show)

instance FromJSON a => FromJSON (Block a) where
    parseJSON (Object v) = do
        number <- v .:? "number"
        hash <- v .:? "hash"
        parentHash <- v .: "parentHash"
        nonce <- v .:? "nonce"
        sha3Uncles <- v .: "sha3Uncles"
        logsBloom <- v .:? "logsBloom"
        transactionsRoot <- v .: "transactionsRoot"
        stateRoot <- v .: "stateRoot"
        receiptsRoot <- v .: "receiptsRoot"
        miner <- v .: "miner"
        difficulty <- v .: "difficulty"
        totalDifficulty <- v .: "totalDifficulty"
        extraData <- v .: "extraData"
        size <- v .: "size"
        gasLimit <- v .: "gasLimit"
        gasUsed <- v .: "gasUsed"
        timestamp <- v .: "timestamp"
        transactions <- v .: "transactions"
        uncles <- v .: "uncles"
        pure Block {
            blockNumber = number,
            blockHash = hash,
            blockParentHash = parentHash,
            blockNonce = nonce,
            blockSha3Uncles = sha3Uncles,
            blockLogsBloom = logsBloom,
            blockTransactionsRoot = transactionsRoot,
            blockStateRoot = stateRoot,
            blockReceiptsRoot = receiptsRoot,
            blockMiner = miner, 
            blockDifficulty = difficulty,
            blockTotalDifficulty = totalDifficulty,
            blockExtraData = extraData,
            blockSize = size,
            blockGasLimit = gasLimit,
            blockGasUsed = gasUsed,
            blockTimestamp = timestamp,
            blockTransactions = transactions,
            blockUncles = uncles
        }
    parseJSON _ = mzero

--
-- Utility
--

encodeEither :: (ToJSON a, ToJSON b) => Either a b -> Value
encodeEither = either toJSON toJSON
