module Test.Oscoin.Crypto.Blockchain.Genesis
    ( tests
    ) where

import           Oscoin.Prelude

import           Data.Aeson (fromJSON, toJSON)
import           Oscoin.Consensus.Nakamoto (PoW, validateBasic)
import           Oscoin.Crypto.Blockchain.Block
import           Oscoin.Crypto.Blockchain.Genesis

import           Oscoin.Test.Consensus.Nakamoto.Arbitrary ()
import           Oscoin.Test.Crypto
import           Oscoin.Test.Crypto.Hash.Arbitrary ()
import           Oscoin.Test.Time ()
import           Test.Oscoin.Crypto.Blockchain.Block.Difficulty.Gen


import           Hedgehog
import           Hedgehog.Gen.QuickCheck
import           Test.Tasty
import           Test.Tasty.Hedgehog

tests :: Dict (IsCrypto c) -> TestTree
tests d = testGroup "Test.Oscoin.Crytpo.Blockchain.Genesis"
    [ testProperty "prop_trippingJsonGenesisParameters" (prop_trippingJsonGenesisParameters d)
    , testProperty "prop_parametersMatchBlock" (prop_parametersMatchBlock d)
    , testProperty "prop_generatedBlockValid" (prop_generatedBlockValid d)
    ]

-- | Test roundtrip of JSON serialization of 'GenesisParameters'
prop_trippingJsonGenesisParameters :: forall c. Dict (IsCrypto c) -> Property
prop_trippingJsonGenesisParameters Dict = property $ do
    genesisParameters <- forAll $ genGenesisParameters @c
    tripping genesisParameters toJSON fromJSON


-- | Assert that genesis parameters match the values in the generated
-- genesis block.
prop_parametersMatchBlock :: forall c. Dict (IsCrypto c) -> Property
prop_parametersMatchBlock Dict = property $ do
    gpTimestamp <- forAll $ arbitrary
    gpStartDifficulty <- forAll $ genTestableDifficulty
    gpBeneficiary <- forAll $ arbitrary
    genesisParameters <- case createGenesisParameters @c gpBeneficiary gpTimestamp gpStartDifficulty of
        Just params -> pure params
        Nothing     -> failure
    let genesisBlock = buildGenesisBlock genesisParameters
    gpBeneficiary === blockBeneficiary genesisBlock
    gpStartDifficulty === blockTargetDifficulty (blockHeader genesisBlock)
    gpTimestamp === blockTimestamp (blockHeader genesisBlock)


-- | Assert that blocks build from genesis parameters are valid genesis
-- blocks.
prop_generatedBlockValid :: forall c. Dict (IsCrypto c) -> Property
prop_generatedBlockValid Dict = property $ do
    gpTimestamp <- forAll $ arbitrary
    gpStartDifficulty <- forAll $ genTestableDifficulty
    gpBeneficiary <- forAll $ arbitrary
    genesisParameters <- case createGenesisParameters @c gpBeneficiary gpTimestamp gpStartDifficulty of
        Just params -> pure params
        Nothing     -> failure
    -- The 'tx' type 'Int32' is chosen arbitrarily because we need a tx
    -- with a 'Serialise' instance. We don’t choose '()' because it is
    -- used in the implementation of 'buildGenesisBlock' and we don’t
    -- what the tests to rely on that fact.
    let genesisBlock :: Block c Int32 (Sealed c PoW) = buildGenesisBlock genesisParameters
    validateBasic genesisBlock === Right ()
    isGenesisBlock genesisBlock === True


genGenesisParameters :: IsCrypto c => Gen (GenesisParameters c)
genGenesisParameters = do
    gpTimestamp <- arbitrary
    gpStartDifficulty <- genDifficulty
    gpBeneficiary <- arbitrary
    gpPoW <- arbitrary
    pure $ GenesisParameters{..}
