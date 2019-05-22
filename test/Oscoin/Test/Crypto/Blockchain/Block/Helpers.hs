module Oscoin.Test.Crypto.Blockchain.Block.Helpers where

import           Oscoin.Crypto.Blockchain.Block (Beneficiary)
import           Oscoin.Crypto.Hash (HasHashing, zeroShortHash)

-- | The default beneficiary.
defaultBeneficiary :: HasHashing c => Beneficiary c
defaultBeneficiary = zeroShortHash
