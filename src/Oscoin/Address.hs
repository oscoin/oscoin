module Oscoin.Address where

import Oscoin.Crypto.Hash
import Oscoin.Crypto.PubKey

type Address = Hashed PublicKey
