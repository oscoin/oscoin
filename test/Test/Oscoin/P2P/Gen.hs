module Test.Oscoin.P2P.Gen
    ( genNetwork
    , genSomeNetwork
    , genHost
    , genHostname
    , genIP
    , genMsg
    , genMsgId
    )
where

import           Oscoin.Prelude

import           Oscoin.Crypto.Blockchain.Block (Sealed, blockHash)
import           Oscoin.Crypto.Hash (hash)
import           Oscoin.P2P.Types
                 ( Host
                 , Hostname
                 , Msg(..)
                 , MsgId(..)
                 , Network(Devnet, Mainnet, Testnet)
                 , namedHost
                 , numericHost
                 , randomNetwork
                 , readHostnameText
                 )

import           Data.IP (IP(..), toIPv4, toIPv6)
import qualified Data.Text as T
import           System.Random.SplitMix (mkSMGen)

import           Oscoin.Test.Crypto
import           Oscoin.Test.Crypto.Blockchain.Block.Arbitrary ()
import           Oscoin.Test.Crypto.Blockchain.Block.Generators
import           Oscoin.Test.Data.Tx.Arbitrary ()

import           Hedgehog (Gen)
import qualified Hedgehog.Gen as Gen
import           Hedgehog.Gen.QuickCheck (quickcheck)
import qualified Hedgehog.Range as Range

genNetwork :: Gen Network
genNetwork = Gen.choice
    [ pure Mainnet
    , pure Testnet
    , pure Devnet
    , genSomeNetwork
    ]

-- | Generate a random 'Somenet'
genSomeNetwork :: Gen Network
genSomeNetwork = do
    rng <- mkSMGen <$> Gen.word64 Range.linearBounded
    pure $ randomNetwork rng

genHost :: Gen Host
genHost = Gen.choice
    [ numericHost <$> genIP
    , namedHost   <$> genHostname
    ]

genHostname :: Gen Hostname
genHostname = do
    hostname <-
        Gen.filter isRight $ do
            labels <-
                Gen.nonEmpty (Range.constant 1 42) $
                    Gen.text (Range.constant 1 63) Gen.alphaNum
            pure . readHostnameText $
                foldr' (\l t -> let t' = l <> "." <> t
                                 in if T.length t' > 253 then t else t')
                       mempty
                       (toList labels)
    case hostname of
        Right x -> pure x
        Left  _ -> panic "Test.Oscoin.P2P.Gen: unexpected Left"

genIP :: Gen IP
genIP = Gen.choice [ ipv4, ipv6 ]
  where
    ipv4 = IPv4 . toIPv4 <$> replicateM 4 (Gen.int (Range.constant 0 255))
    ipv6 = IPv6 . toIPv6 <$> replicateM 8 (Gen.int (Range.constant 0 65535))

genMsg :: IsCrypto c => Gen (Msg c Text (Sealed c Text))
genMsg = Gen.choice [ blkMsg, txMsg ]
  where
    blkMsg = BlockMsg <$> quickcheck genStandaloneBlock
    txMsg  = TxMsg    <$> Gen.text (Range.constant 0 512) Gen.unicodeAll

genMsgId :: forall c. IsCrypto c => Gen (MsgId c Text)
genMsgId = Gen.choice [ blkMsgId, txMsgId ]
  where
    blkMsgId = BlockId . blockHash <$> quickcheck (genStandaloneBlock @c @Text @(Sealed c Text))
    txMsgId  = TxId    . hash      <$> Gen.text (Range.constant 0 512) Gen.unicodeAll
