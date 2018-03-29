module Crypto.Data.Auth.Tree.Proof where

import           Prelude hiding (lookup, null, elem, foldr, map, traverse, pred, succ, last)
import           Crypto.Data.Auth.Tree hiding (lookup)
import           Crypto.Data.Auth.Tree.Internal
import           Crypto.Hash (HashAlgorithm, Digest)
import           Data.ByteArray (ByteArrayAccess)
import           Data.List (intercalate)
import qualified Data.List as List

type Error = String

data Proof d k v =
      KeyExistsProof (Path d ())
      -- ^ Proof of existence of a key.
    | KeyAbsentProof (Maybe (Path d (k, v))) (Maybe (Path d (k, v)))
      -- ^ Proof of absence of a key.
    deriving (Eq)

instance (Show k, Show v, Show d) => Show (Proof d k v) where
    show (KeyExistsProof path) =
        "KeyExistsProof\n" ++ show path
    show (KeyAbsentProof l r) =
        "KeyAbsentProof\n" ++ show l ++ "\n" ++ show r

data Path d a = Path
    { pathElems :: [PathElem d]
    , pathLeaf  :: a
    } deriving (Eq)

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

    -- | If the given paths have a common prefix branch, return them without
    -- the path elements in common.
    stripCommonPrefix :: [PathElem a] -> [PathElem a] -> ([PathElem a], [PathElem a])
    stripCommonPrefix (reverse -> ls) (reverse -> rs) =
        let (l, r) = go ls rs in
            ( reverse l
            , reverse r
            )
      where
        go (l:ls) (r:rs)
            | l == r    = go ls rs
            | otherwise = (ls, rs)
        go ls rs = (ls, rs)

isLeftmost :: [PathElem a] -> Bool
isLeftmost (R _ : xs) = isLeftmost xs
isLeftmost []         = True
isLeftmost _          = False

isRightmost :: [PathElem a] -> Bool
isRightmost (L _ : xs) = isRightmost xs
isRightmost []         = True
isRightmost _          = False

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
    -> Either Error ()   -- ^ A 'Right' value signifies success.
verify (KeyExistsProof (Path xs ())) root k (Just v) =
    assert (pathDigest xs k v == root) "Path should match supplied root"
verify (KeyExistsProof _) _ _ Nothing =
    Left "KeyExistsProof requires a value to be verified"
-- Key is absent in an empty tree.
verify (KeyAbsentProof Nothing Nothing) root _ Nothing =
    assert (root == emptyHash) "Root hash should be empty"
-- Key is greater than the maximum value in the tree.
verify (KeyAbsentProof (Just (Path xs (k, v))) Nothing) root k' Nothing = do
    assert (pathDigest xs k v == root) "Left path should match supplied root"
    assert (k' > k)                    "Left path leaf should be lesser than key"
    assert (isRightmost xs)            "Left path should be rightmost"
-- Key is lesser than the minimum vlaue in the tree.
verify (KeyAbsentProof Nothing (Just (Path xs (k, v)))) root k' Nothing = do
    assert (k' < k)                    "Right path leaf should be greater than key"
    assert (pathDigest xs k v == root) "Right path should match supplied root"
    assert (isLeftmost xs)             "Right path should be leftmost"
-- Key is between two existing values in the tree.
verify (KeyAbsentProof (Just (Path ls (lk, lv))) (Just (Path rs (rk, rv)))) root k Nothing = do
    assert (lk < k && k < rk)            "Key should be between the left and right paths"
    assert (pathDigest ls lk lv == root) "Left path should match supplied root"
    assert (pathDigest rs rk rv == root) "Right path should match supplied root"
    assert (adjacent ls rs)              "Left and right paths should be adjacent"
verify (KeyAbsentProof _ _) _ _ (Just _) =
    Left "A value was passed to verify a KeyAbsentProof"

assert :: Bool -> String -> Either Error ()
assert True  _   = Right ()
assert False err = Left err

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
