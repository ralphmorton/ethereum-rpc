{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Ethereum.RPC.Data (
    EthHex(..),
    ABI(..),
    Quantity,
    unQuantity,
    mkQuantity,
    Unformatted(..),
    Bytes,
    unBytes,
    mkBytes,
    Function(..),
    Address,
    unAddress,
    mkAddress,
    Hash,
    unHash,
    mkHash,
    Signature(..),
    Event(..),
    Network(..),
    SyncStatus(..),
    SyncState(..),
    TransactionIn(..),
    TransactionOut(..),
    TransactionReceipt(..),
    Call(..),
    Filter(..),
    BlockTag(..),
    BlockRef(..),
    Block(..),
    Compiler(..),
    FilterResult(..)
) where

import Prelude hiding (take)

import Control.Applicative ((<|>))
import Control.Monad (mzero, void, when)
import Data.Aeson hiding (decode, decode', encode)
import Data.Attoparsec.ByteString hiding (takeWhile)
import Data.Bits
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B
import Data.Int
import Data.Monoid (Monoid, (<>))
import Data.Proxy
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Word
import GHC.TypeLits
import Numeric (readHex, showHex)

class EthHex a where
    toEthHexString :: a -> T.Text
    fromEthHexString :: T.Text -> Maybe a

class ABI a where
    dynamic :: Proxy a -> Bool
    decode :: Parser a
    encode :: a -> BS.ByteString

--
-- Quantity
--

newtype Quantity
    = Quantity { unQuantity :: Integer }
    deriving (Eq, Ord, Show)

instance EthHex Quantity where
    toEthHexString = T.toLower . T.pack . ("0x"<>) . flip showHex "" . unQuantity
    fromEthHexString s | T.isPrefixOf "0x" s = do
        let encoded = drop 2 (T.unpack s)
        case readHex encoded of
            [(q, "")] -> pure (Quantity q)
            _ -> mzero
    fromEthHexString _ = mzero

instance ToJSON Quantity where
    toJSON q = String (toEthHexString q)

instance FromJSON Quantity where
    parseJSON (String s) = maybe mzero pure (fromEthHexString s)
    parseJSON _ = mzero

mkQuantity :: Integer -> Maybe Quantity
mkQuantity i | i >= 0 = Just (Quantity i)
mkQuantity _ = mzero

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
    show u = "Unformatted { toEthHexString = " <> T.unpack (toEthHexString u) <> " }"

instance EthHex Unformatted where
    toEthHexString = T.toLower . T.pack . B.unpack . ("0x"<>) . B16.encode . unUnformatted
    fromEthHexString s | T.isPrefixOf "0x" s = do
        let encoded = (B.pack . drop 2) (T.unpack s)
        case B16.decode encoded of
            (v, "") -> pure (Unformatted v)
            _ -> mzero
    fromEthHexString _ = mzero

instance ToJSON Unformatted where
    toJSON (Unformatted v) = String ("0x" <> hex)
        where hex = (T.pack . B.unpack) (B16.encode v)

instance FromJSON Unformatted where
    parseJSON (String s) = maybe mzero pure (fromEthHexString s)
    parseJSON _ = mzero

--
-- Bytes
--

newtype Bytes (n :: Nat)
    = Bytes { unBytes :: BS.ByteString }
    deriving Eq

instance (KnownNat n, n <= 32) => Show (Bytes n) where
    show bx = "Bytes { toEthHexString = " <> T.unpack (toEthHexString bx) <> " }"

instance (KnownNat n, n <= 32) => EthHex (Bytes n) where
    toEthHexString = T.toLower . T.pack . B.unpack . ("0x"<>) . B16.encode . padR 32 . unBytes
    fromEthHexString s = do
        let proxy = undefined :: KnownNat n => Bytes n
        let n = fromIntegral (natVal proxy)
        let encoded = (B.pack . T.unpack . T.take (n * 2)) (T.drop 2 s)
        let rest = T.drop (n * 2 + 2) s
        case (B16.decode encoded, rest == T.replicate (64 - n * 2) "0") of
            ((bs, ""), True) -> pure (setBytes proxy bs)
            _ -> mzero

instance (KnownNat n, n <= 32) => ABI (Bytes n) where
    dynamic = const False
    decode = do
        let proxy = undefined :: KnownNat n => Bytes n
        let n = fromIntegral (natVal proxy)
        bs <- take n
        when (n > 0) (void . take $ 32 - n)
        pure (setBytes proxy bs)
    encode = padR 32 . unBytes

mkBytes :: (KnownNat n, n <= 32) => BS.ByteString -> Maybe (Bytes n)
mkBytes bs
    | BS.length bs == n = Just (setBytes proxy bs)
    | otherwise = Nothing
    where
    proxy = undefined :: KnownNat n => Bytes n
    n = fromIntegral (natVal proxy)

setBytes :: Bytes n -> BS.ByteString -> Bytes n
setBytes _ = Bytes

--
-- Function
--

newtype Function
    = Function { unFunction :: Bytes 24 }
    deriving Eq

instance Show Function where
    show (Function bx) = "Function { toEthHexString = " <> T.unpack (toEthHexString bx) <> " }"

instance EthHex Function where
    toEthHexString = toEthHexString . unFunction
    fromEthHexString = fmap Function . fromEthHexString

instance ABI Function where
    dynamic = const False
    decode = Function <$> decode
    encode = encode . unFunction

--
-- Address
--

newtype Address
    = Address { unAddress :: B.ByteString }
    deriving Eq

instance Show Address where
    show a = "Address { toEthHexString = " <> T.unpack (toEthHexString a) <> " }"

instance EthHex Address where
    toEthHexString = T.toLower . T.pack . B.unpack . ("0x"<>) . B16.encode . unAddress
    fromEthHexString s = do
        Unformatted bs <- fromEthHexString s
        case BS.length bs of
            20 -> pure (Address bs)
            _ -> mzero

instance ABI Address where
    dynamic = const False
    decode = Address . BS.drop 12 <$> take 32
    encode = padL 32 . unAddress

instance ToJSON Address where
    toJSON = toJSON . toEthHexString

instance FromJSON Address where
    parseJSON (String s) = maybe mzero pure (fromEthHexString s)
    parseJSON _ = mzero

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
    show h = "Hash { toEthHexString = " <> T.unpack (toEthHexString h) <> " }"

instance EthHex Hash where
    toEthHexString = T.toLower . T.pack . B.unpack . ("0x"<>) . B16.encode . unHash
    fromEthHexString s = do
        Unformatted bs <- fromEthHexString s
        case BS.length bs of
            32 -> pure (Hash bs)
            _ -> mzero

instance ABI Hash where
    dynamic = const False
    decode = Hash <$> take 32
    encode = unHash

instance ToJSON Hash where
    toJSON = toJSON . toEthHexString

instance FromJSON Hash where
    parseJSON (String s) = maybe mzero pure (fromEthHexString s)
    parseJSON _ = mzero

mkHash :: B.ByteString -> Maybe Hash
mkHash s | B.length s == 32 = Just (Hash s)
mkHash _ = Nothing

--
-- Signature
--

data Signature = Signature {
    sigR :: Integer,
    sigS :: Integer,
    sigV :: Word8
} deriving (Eq, Show)
    
instance EthHex Signature where
    toEthHexString sig = toEthHexString (Unformatted bs)
        where
        bs = r <> s <> v
        r = encodeInt (sigR sig)
        s = encodeInt (sigS sig)
        v = i2bs (fromIntegral $ sigV sig)
    fromEthHexString str = do
        Unformatted bs <- fromEthHexString str
        case BS.length bs == 65 of
            False -> mzero
            True -> do
                let r = bs2i (BS.take 32 bs)
                let s = bs2i (BS.take 32 $ BS.drop 32 bs)
                let v = fromIntegral $ bs2i (BS.drop 64 bs)
                pure (Signature r s v)

instance ToJSON Signature where
    toJSON = toJSON . toEthHexString

instance FromJSON Signature where
    parseJSON (String s) = maybe mzero pure (fromEthHexString s)
    parseJSON _ = mzero 

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
-- TransactionReceipt
--

data TransactionReceipt = TransactionReceipt {
    transRecTransactionHash :: Hash,
    transRecTransactionIndex :: Quantity,
    transRecBlockHash :: Hash,
    transRecBlockNumber :: Quantity,
    transRecCumulativeGasUsed :: Quantity,
    transRecGasUsed :: Quantity,
    transRecContractAddress :: Maybe Address,
    transRecLogs :: [Event]
} deriving (Eq, Show)

instance FromJSON TransactionReceipt where
    parseJSON (Object v) = do
        transactionHash <- v .: "transactionHash"
        transactionIndex <- v .: "transactionIndex"
        blockHash' <- v .: "blockHash"
        blockNumber' <- v .: "blockNumber"
        cumulativeGasUsed <- v .: "cumulativeGasUsed"
        gasUsed <- v .: "gasUsed"
        contractAddress <- v .:? "contractAddress"
        logs <- v .: "logs"
        pure TransactionReceipt {
            transRecTransactionHash = transactionHash,
            transRecTransactionIndex = transactionIndex,
            transRecBlockHash = blockHash',
            transRecBlockNumber = blockNumber',
            transRecCumulativeGasUsed = cumulativeGasUsed,
            transRecGasUsed = gasUsed,
            transRecContractAddress = contractAddress,
            transRecLogs = logs
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
    filterFromBlock :: Maybe BlockRef,
    filterToBlock :: Maybe BlockRef,
    filterAddress :: Maybe (Either Address [Address]),
    filterTopics :: Maybe [Unformatted]
} deriving (Eq, Show)

instance ToJSON Filter where
    toJSON f = object ["fromBlock" .= from, "toBlock" .= to, "address" .= addr, "topics" .= topics]
        where
        from = filterFromBlock f
        to = filterToBlock f
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
-- Compiler
--

data Compiler
    = Solidity
    | LLL
    | Serpent
    | UnknownCompiler
    deriving (Eq, Show)

instance FromJSON Compiler where
    parseJSON (String "solidity") = pure Solidity
    parseJSON (String "lll") = pure LLL
    parseJSON (String "serpent") = pure Serpent
    parseJSON _ = mzero

--
-- FilterResult
--

data FilterResult
    = Hashes [Hash]
    | Events [Event]
    deriving (Eq, Show)

instance FromJSON FilterResult where
    parseJSON v = (Hashes <$> parseJSON v) <|> (Events <$> parseJSON v)

--
-- Marshal instances
--

instance (ABI a, ABI b, ABI c, ABI d, ABI e) => ABI (a, b, c, d, e) where
    dynamic = const True
    decode = do
        ha <- decodeH Proxy
        hb <- decodeH Proxy
        hc <- decodeH Proxy
        hd <- decodeH Proxy
        he <- decodeH Proxy
        a <- maybe decode pure ha
        b <- maybe decode pure hb
        c <- maybe decode pure hc
        d <- maybe decode pure hd
        e <- maybe decode pure he
        pure (a, b, c, d, e)
    encode (a, b, c, d, e) = ha <> hb <> hc <> hd <> he <> ta <> tb <> tc <> td <> te
        where
        (ha, ta) = encodeHT 64 a
        aLen = BS.length (ha <> ta) - 32 + 64
        (hb, tb) = encodeHT (fromIntegral aLen) b
        bLen = BS.length (hb <> tb) - 32 + aLen
        (hc, tc) = encodeHT (fromIntegral bLen) c 
        cLen = BS.length (hc <> tc) - 32 + bLen
        (hd, td) = encodeHT (fromIntegral cLen) d 
        dLen = BS.length (hd <> td) - 32 + cLen
        (he, te) = encodeHT (fromIntegral dLen) e

instance (ABI a, ABI b, ABI c, ABI d) => ABI (a, b, c, d) where
    dynamic = const True
    decode = do
        ha <- decodeH Proxy
        hb <- decodeH Proxy
        hc <- decodeH Proxy
        hd <- decodeH Proxy
        a <- maybe decode pure ha
        b <- maybe decode pure hb
        c <- maybe decode pure hc
        d <- maybe decode pure hd
        pure (a, b, c, d)
    encode (a, b, c, d) = ha <> hb <> hc <> hd <> ta <> tb <> tc <> td
        where
        (ha, ta) = encodeHT 64 a
        aLen = BS.length (ha <> ta) - 32 + 64
        (hb, tb) = encodeHT (fromIntegral aLen) b
        bLen = BS.length (hb <> tb) - 32 + aLen
        (hc, tc) = encodeHT (fromIntegral bLen) c 
        cLen = BS.length (hc <> tc) - 32 + bLen
        (hd, td) = encodeHT (fromIntegral cLen) d 

instance (ABI a, ABI b, ABI c) => ABI (a, b, c) where
    dynamic = const True
    decode = do
        ha <- decodeH Proxy
        hb <- decodeH Proxy
        hc <- decodeH Proxy
        a <- maybe decode pure ha
        b <- maybe decode pure hb
        c <- maybe decode pure hc
        pure (a, b, c)
    encode (a, b, c) = ha <> hb <> hc <> ta <> tb <> tc
        where
        (ha, ta) = encodeHT 64 a
        aLen = BS.length (ha <> ta) - 32 + 64
        (hb, tb) = encodeHT (fromIntegral aLen) b
        bLen = BS.length (ha <> ta) - 32 + aLen
        (hc, tc) = encodeHT (fromIntegral bLen) c 

instance (ABI a, ABI b) => ABI (a, b) where
    dynamic = const True
    decode = do
        ha <- decodeH Proxy
        hb <- decodeH Proxy
        a <- maybe decode pure ha
        b <- maybe decode pure hb
        pure (a, b)
    encode (a, b) = ha <> hb <> ta <> tb
        where
        (ha, ta) = encodeHT 64 a
        aLen = BS.length (ha <> ta) - 32 + 64
        (hb, tb) = encodeHT (fromIntegral aLen) b

decodeH :: ABI a => Proxy a -> Parser (Maybe a)
decodeH p
    | dynamic p = take 32 *> pure Nothing
    | otherwise = Just <$> decode

encodeHT :: ABI a => Word64 -> a -> (BS.ByteString, BS.ByteString)
encodeHT offset v = case dynamic (prox v) of
    False -> (encode v, "")
    True -> (encode offset, encode v)

prox :: a -> Proxy a
prox = const Proxy

instance ABI a => ABI [a] where
    dynamic = const True
    decode = do
        n <- fromIntegral . bs2i <$> take 32
        count n decode
    encode ax = encodeInt (length ax) <> mconcat (encode <$> ax)

instance ABI B.ByteString where
    dynamic = const True
    decode = do
        n <- fromIntegral . bs2i <$> take 32
        bs <- take n
        void (take $ calculatePad n)
        pure bs
        where
        calculatePad n
            | mod n 32 == 0 = 0
            | otherwise = 32 - mod n 32
    encode bs = encodeInt (BS.length bs) <> padR 32 bs

instance ABI T.Text where
    dynamic = const True
    decode = T.decodeUtf8 <$> decode
    encode = encode . T.encodeUtf8

instance ABI Bool where
    dynamic = const False
    decode = do
        n <- (decode :: Parser Word8)
        case n of
            0 -> pure False
            1 -> pure True
            _ -> mzero
    encode True = encode (1 :: Word8)
    encode False = encode (0 :: Word8)

instance ABI Int8 where
    dynamic = const False
    decode = do
        bs <- take 32
        (pure . fromIntegral) (bs2i bs)
    encode = encodeInt

instance ABI Int16 where
    dynamic = const False
    decode = do
        bs <- take 32
        (pure . fromIntegral) (bs2i bs)
    encode = encodeInt

instance ABI Int32 where
    dynamic = const False
    decode = do
        bs <- take 32
        (pure . fromIntegral) (bs2i bs)
    encode = encodeInt

instance ABI Int64 where
    dynamic = const False
    decode = do
        bs <- take 32
        (pure . fromIntegral) (bs2i bs)
    encode = encodeInt

instance ABI Word8 where
    dynamic = const False
    decode = do
        bs <- take 32
        (pure . fromIntegral) (bs2i bs)
    encode = encodeInt

instance ABI Word16 where
    dynamic = const False
    decode = do
        bs <- take 32
        (pure . fromIntegral) (bs2i bs)
    encode = encodeInt

instance ABI Word32 where
    dynamic = const False
    decode = do
        bs <- take 32
        (pure . fromIntegral) (bs2i bs)
    encode = encodeInt

instance ABI Word64 where
    dynamic = const False
    decode = do
        bs <- take 32
        (pure . fromIntegral) (bs2i bs)
    encode = encodeInt

encodeInt :: Integral a => a -> BS.ByteString
encodeInt i
    | i >= 0 = padL 32 (i2bs $ fromIntegral i)
    | otherwise = BS.pack (complement <$> bx)
        where bx = BS.unpack (padL 32 . i2bs . fromIntegral $ abs i - 1)

padL :: Int -> BS.ByteString -> BS.ByteString
padL n bs
    | mod (BS.length bs) n == 0 = bs
    | otherwise = BS.replicate (n - mod (BS.length bs) n) 0 <> bs

padR :: Int -> BS.ByteString -> BS.ByteString
padR n bs
    | mod (BS.length bs) n == 0 = bs
    | otherwise = bs <> BS.replicate (n - mod (BS.length bs) n) 0

bs2i :: BS.ByteString -> Integer
bs2i b
    | sign = go b - 2 ^ (BS.length b * 8)
    | otherwise = go b
    where
    go = BS.foldl' (\i b' -> (i `shiftL` 8) + fromIntegral b') 0
    sign = BS.index b 0 > 127

i2bs :: Integer -> BS.ByteString
i2bs x
    | x == 0 = BS.singleton 0
    | x < 0 = i2bs $ 2 ^ (8 * bytes) + x
    | otherwise = BS.reverse $ BS.unfoldr go x
    where
    bytes = (integerLogBase 2 (abs x) + 1) `quot` 8 + 1
    go i =
        if i == 0 then Nothing
        else Just (fromIntegral i, i `shiftR` 8)

integerLogBase :: Integer -> Integer -> Int
integerLogBase b i =
    if i < b then
        0
    else
        let l = 2 * integerLogBase (b*b) i
            doDiv :: Integer -> Int -> Int
            doDiv i' l' = if i' < b then l' else doDiv (i' `div` b) (l'+1)
        in doDiv (i `div` (b^l)) l

--
-- Utility
--

encodeEither :: (ToJSON a, ToJSON b) => Either a b -> Value
encodeEither = either toJSON toJSON
