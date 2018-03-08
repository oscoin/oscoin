module Crypto.Data.Auth.Tree.Proof where

import           Prelude hiding (lookup, null, elem, foldr, map, traverse, pred, succ, last)
import           Crypto.Data.Auth.Tree hiding (lookup)
import           Crypto.Data.Auth.Tree.Internal
import           Crypto.Hash (HashAlgorithm, Digest)
import           Data.ByteArray (ByteArrayAccess)
import           Data.List (intercalate)
import qualified Data.List as List

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
            , intercalate "\n" (fmap show es)
            ]

data PathElem d =
      L (Digest d)
    | R (Digest d)
    deriving (Show, Eq)

adjacent :: [PathElem a] -> [PathElem a] -> Bool
adjacent l' r' =
    isRightmost l && isLeftmost r
  where
    (l, r) = stripCommonPrefix l' r'

    isLeftmost (R _ : xs) = isLeftmost xs
    isLeftmost (L _ : []) = True
    isLeftmost _          = False

    isRightmost (L _ : xs) = isRightmost xs
    isRightmost (R _ : []) = True
    isRightmost _          = False

    -- | If the given paths have a common prefix branch, return them without
    -- the path elements in common.
    stripCommonPrefix :: [PathElem a] -> [PathElem a] -> ([PathElem a], [PathElem a])
    stripCommonPrefix (reverse -> ls) (reverse -> rs) =
        let (l, r) = go ls rs in (reverse l, reverse r)
      where
        go (l:ls) (r:rs)
            | l == r    = go ls rs
            | otherwise = (l:ls, r:rs)
        go ls rs = (ls, rs)

pathDigest
    :: (ByteArrayAccess k, ByteArrayAccess v, HashAlgorithm a)
    => [PathElem a] -> k -> v -> Digest a
pathDigest elems k v =
    List.foldl' (flip digest) (hashLeaf k v) elems
  where
    digest (L l) r = hashNode l r
    digest (R r) l = hashNode l r

-- | Verify a proof.
verify
    :: (HashAlgorithm a, Ord k, ByteArrayAccess k, ByteArrayAccess v)
    => Proof a k v       -- ^ The proof to verify.
    -> Digest a          -- ^ The root hash.
    -> k                 -- ^ The key to verify.
    -> Maybe v           -- ^ The value to verify in case of a proof of existence.
    -> Either String ()  -- ^ A 'Right' value signifies success.
verify (KeyExistsProof (Path xs ())) root k (Just v)
    | pathDigest xs k v == root = Right ()
    | otherwise                 = Left "Roots don't match"
verify (KeyAbsentProof Nothing Nothing) root _ Nothing
    | root == emptyHash = Right ()
    | otherwise         = Left "Empty absence proof"
-- Key is greater than the maximum value in the tree.
verify (KeyAbsentProof (Just (Path xs (k, v))) Nothing) root k' Nothing
    | pathDigest xs k v == root
    , k' > k
    = Right ()

    | otherwise
    = Left "Roots don't match"
-- Key is lesser than the minimum vlaue in the tree.
verify (KeyAbsentProof Nothing (Just (Path xs (k, v)))) root k' Nothing
    | pathDigest xs k v == root
    , k' < k
    = Right ()

    | otherwise
    = Left "Roots don't match"
verify (KeyAbsentProof (Just (Path ls (lk, lv)))
                       (Just (Path rs (rk, rv)))) root k Nothing
    | lk < k && k < rk                            -- `k` is between the left and right paths.
    , pathDigest ls lk lv == root                 -- The left path is valid.
    , pathDigest rs rk rv == root                 -- The right path is valid.
    , adjacent ls rs                              -- The left and right path are adjacent.
    = Right ()

    | otherwise = Left "Invalid proof"
verify _ _ _ _ = undefined

lookupProof
    :: (HashAlgorithm a, Ord k, ByteArrayAccess k, ByteArrayAccess v)
    => k
    -> Tree k v
    -> (Maybe v, Proof a k v)
lookupProof k tree =
    f k tree []
  where
    f k (Leaf k' v) path
        | k == k'   = (Just v, KeyExistsProof (Path path ()))
        | otherwise = (Nothing, keyAbsentProof k tree)
    f k (Node k' l r) path
        | k < k'    = f k l (R (merkleHash r) : path)
        | otherwise = f k r (L (merkleHash l) : path)
    f _ Empty _ =
        (Nothing, KeyAbsentProof Nothing Nothing)
{-# INLINABLE lookupProof #-}

keyAbsentProof :: (HashAlgorithm a, ByteArrayAccess k, ByteArrayAccess v, Ord k) => k -> Tree k v -> Proof a k v
keyAbsentProof k tree =
    KeyAbsentProof left right
  where
    left = do
        (k', v') <- pred k tree
        (_, KeyExistsProof path) <- pure $ lookupProof k' tree
        pure (path { pathLeaf = (k', v') })
    right = do
        (k', v') <- succ k tree
        (_, KeyExistsProof path) <- pure $ lookupProof k' tree
        pure (path { pathLeaf = (k', v') })
