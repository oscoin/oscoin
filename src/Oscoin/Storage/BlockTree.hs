-- | An opaque 'BlockTree', composed by its public and private API. This is
-- the main module meant to be imported and used throughout the codebase. For
-- a description of the types themselves, see the '.Internal' module.

module Oscoin.Storage.BlockTree
    ( BlockTree
    , BlockTreeReader
    , BlockTreeWriter

    , hoistBlockTreeReader
    , hoistBlockTreeWriter
    , hoistBlockTree
    , insertBlocksNaive
    , isNovelBlock
    ) where

import           Oscoin.Prelude

import           Oscoin.Crypto.Blockchain.Block
import           Oscoin.Storage.BlockTree.Internal
import           Oscoin.Time.Chrono

{------------------------------------------------------------------------------
Extra operations on the BlockTree, which are implementation-independent.
------------------------------------------------------------------------------}

-- | Given a natural transformation from @n@ to @m@, hoists a 'BlockTree'
-- initialised with a monad @n@ to work in the monad @m@.
hoistBlockTreeReader
    :: forall c tx s n m. (forall a. n a -> m a)
    -> BlockTreeReader c tx s n
    -> BlockTreeReader c tx s m
hoistBlockTreeReader natTrans bs = BlockTreeReader
    { getGenesisBlock       = natTrans (getGenesisBlock bs)
    , lookupBlock           = natTrans . lookupBlock bs
    , lookupTx              = natTrans . lookupTx bs
    , getBlocksByDepth      = natTrans . getBlocksByDepth bs
    , getBlocksByParentHash = natTrans . getBlocksByParentHash bs
    , getTip                = natTrans (getTip bs)
    }

hoistBlockTreeWriter
    :: forall c tx s n m. (forall a. n a -> m a)
    -> BlockTreeWriter c tx s n
    -> BlockTreeWriter c tx s m
hoistBlockTreeWriter natTrans bs = BlockTreeWriter
    { insertBlock  = natTrans . insertBlock bs }

hoistBlockTree
    :: forall c tx s n m. (forall a. n a -> m a)
    -> BlockTree c tx s n
    -> BlockTree c tx s m
hoistBlockTree natTrans (public, private) =
  (hoistBlockTreeReader natTrans public, hoistBlockTreeWriter natTrans private)


-- | /O(n)/. A naive function to store blocks in linear time.
-- Useful for testing but discouraged for any serious production use.
-- NOTE(adn): It might be conceivable to have this function be upgraded as
-- a proper abstract function of the 'BlockTree', so that different implementations
-- can decide how to implement it efficiently.
insertBlocksNaive
    :: Monad m
    => BlockTreeWriter c tx s m
    -> OldestFirst [] (Block c tx (Sealed c s))
    -> m ()
insertBlocksNaive bs = traverse_ (insertBlock bs)

isNovelBlock :: Functor m => BlockTreeReader c tx s m -> BlockHash c -> m Bool
isNovelBlock bs = map isNothing . lookupBlock bs
