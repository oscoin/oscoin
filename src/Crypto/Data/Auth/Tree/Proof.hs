module Crypto.Data.Auth.Tree.Proof where

import Prelude

import Crypto.Hash (Digest)
import Data.List (intercalate)

data Proof d k v =
      KeyExistsProof (Path d ())
      -- ^ Proof of existence of a key.
    | KeyAbsentProof (Maybe (Path d (k, v))) (Maybe (Path d (k, v)))
      -- ^ Proof of absence of a key.

instance (Show k, Show v, Show d) => Show (Proof d k v) where
    show (KeyExistsProof path) =
        "KeyExistsProof\n" ++ show path
    show (KeyAbsentProof l r) =
        "KeyAbsentProof\n" ++ show l ++ "\n" ++ show r

data Path d a = Path
    { pathElems :: [PathElem d]
    , pathLeaf  :: a
    }

instance (Show d, Show a) => Show (Path d a) where
    show (Path es leaf) =
        unlines
            [ show leaf
            , intercalate "\n" (map show es)
            ]

data PathElem d =
      L (Digest d)
    | R (Digest d)
    deriving (Show)

