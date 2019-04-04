module Test.Oscoin.Configuration.Gen
    ( genNetwork
    , genSomeNetwork
    )
where

import           Oscoin.Prelude

import           Oscoin.Configuration (Network(..), readNetwork)

import           Hedgehog (Gen)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

genNetwork :: Gen Network
genNetwork = Gen.choice
    [ pure Mainnet
    , pure Testnet
    , pure Devnet
    , genSomeNetwork
    ]

-- | Generate a random 'SomeNetwork'
genSomeNetwork :: Gen Network
genSomeNetwork = do
    net <-
        Gen.filter isRight $
            readNetwork <$> Gen.string (Range.constant 1 63) Gen.alphaNum
    case net of
        Right x -> pure x
        Left  _ -> panic "Test.Oscoin.Configuration.Gen: unexpected Left"
