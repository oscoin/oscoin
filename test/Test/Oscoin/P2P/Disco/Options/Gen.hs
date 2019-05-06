module Test.Oscoin.P2P.Disco.Options.Gen
    ( genOptions
    , genOptNetwork
    , genSeed
    )
where

import           Oscoin.Prelude

import           Oscoin.P2P.Disco.Options
                 (OptNetwork(..), Options(..), evalYesNo)
import           Oscoin.P2P.Types
                 ( Network(..)
                 , NodeAddr(..)
                 , SeedAddr
                 , renderHostname
                 , renderNetwork
                 )

import           Test.Oscoin.P2P.Gen

import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

genOptions :: MonadGen m => m (Options c OptNetwork)
genOptions = Options
    <$> genOptNetwork
    <*> Gen.list (Range.constant 0 5) genSeed
    <*> Gen.list (Range.constant 0 1)
            (toS . renderHostname <$> genHostname)
    <*> Gen.bool
    <*> Gen.bool
    <*> pure Nothing

genOptNetwork :: MonadGen m => m OptNetwork
genOptNetwork = Gen.choice [confirm, noConfirm, pure Random]
  where
    confirm = do
        net <- Gen.element [Mainnet, Testnet]
        pure $ Confirm (renderNetwork net) (evalYesNo net)

    noConfirm =
        NoConfirm <$> Gen.choice [genSomeNetwork, pure Devnet]

genSeed :: MonadGen m => m (SeedAddr c)
genSeed = NodeAddr Nothing <$> genHost <*> genPortNumber
