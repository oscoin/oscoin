module Oscoin.Crypto
    ( Crypto
    , MockCrypto(..)
    ) where

-- | This empty data type serves as a type-level annotation for the cryptography
-- implementation currently in use. Here 'Crypto' denotes the production crypto,
-- which provides robust and safe implementations for both hashing and digital
-- signature schemes.
data Crypto

-- | The mock crypto we use in tests, which provides a fast, non-cryptographic-secure
-- hashing and a very simple and fast digital signature scheme based on bit xoring.
data MockCrypto = MockCrypto
