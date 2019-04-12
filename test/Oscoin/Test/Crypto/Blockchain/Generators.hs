module Oscoin.Test.Crypto.Blockchain.Generators
    ( ForkParams(..)

      -- * Generating generic chains
    , genEvaledBlockchainFrom
    , genEvaledBlockchain
    , genBlockchain
    , genBlockchainFrom
    , genOrphanChainsFrom

      -- * Generating Nakamoto chains
    , genNakamotoBlockchain
    ) where

import           Oscoin.Prelude

import           Codec.Serialise (Serialise)
import           Data.List.NonEmpty ((<|))
import           GHC.Natural

import qualified Oscoin.Consensus.Nakamoto as Nakamoto
import           Oscoin.Crypto.Blockchain
import           Oscoin.Crypto.Blockchain.Eval
import qualified Oscoin.Crypto.Hash as Crypto
import qualified Oscoin.Time.Chrono as Chrono

import           Oscoin.Test.Crypto
import           Oscoin.Test.Crypto.Blockchain.Block.Arbitrary ()
import           Oscoin.Test.Crypto.Blockchain.Block.Generators
import           Test.QuickCheck

-- | Generates a 'Blockchain' starting with the provided genesis block,
-- the state associated with the genesis block, and the transaction
-- evaluator.
--
-- The state hashes of the blocks are correctly computed by applying
-- the evaluator to the block’s transactions.
--
-- Blocks are generated using 'genBlockWith' and include an arbitrary
-- list of transactions. If failing transactions are generated they are
-- included in the block.
--
-- The generated blockchain has length between 2 and 15 depending on
-- the size.
genEvaledBlockchainFrom
    :: ( IsCrypto c
       , Arbitrary tx
       , Arbitrary s
       , Serialise tx
       , Serialise s
       , Crypto.Hashable c st
       )
    => Block c tx (Sealed c s)
    -> Evaluator st tx o
    -> st
    -> Gen (Blockchain c tx s)
genEvaledBlockchainFrom genBlock = genEvaledBlockchainSizedFrom 15 genBlock


-- | Same as 'genEvaledBlockchainFrom' but allows you to specify the
-- maximum length of the blockchain.
--
-- We ensure that there are at least two blocks in the blockchain.
genEvaledBlockchainSizedFrom
    :: ( IsCrypto c
       , Arbitrary tx
       , Arbitrary s
       , Serialise tx
       , Serialise s
       , Crypto.Hashable c st
       )
    => Int
    -> Block c tx (Sealed c s)
    -> Evaluator st tx o
    -> st
    -> Gen (Blockchain c tx s)
genEvaledBlockchainSizedFrom maxLength genBlock eval genState = sized $ \size -> do
    -- Some tests will fail if a blockchain of length 1 is generated.
    let maxLengthSized = 2 `max` ((maxLength * size) `div` 100)
    len <- choose (2, maxLengthSized)
    fst <$> foldM mkNextBlock (fromGenesis genBlock, genState) [1 .. len]
  where
    mkNextBlock (chain, prevSt) _ = do
        txs <- arbitrary
        let (_, st) = evalTraverse eval txs prevSt
        newTip <- genBlockWith (tip chain) txs st
        pure (newTip |> chain, st)


-- | Same as 'genEvaledBlockchainFrom' with an empty genesis block.
genEvaledBlockchain
    :: ( IsCrypto c
       , Arbitrary tx
       , Arbitrary s
       , Serialise tx
       , Serialise s
       , Crypto.Hashable c st
       )
    => Evaluator st tx o
    -> st
    -> Gen (Blockchain c tx s)
genEvaledBlockchain eval initialState = do
    genesisBlock <- genEmptyGenesisBlock initialState
    genEvaledBlockchainFrom genesisBlock eval initialState


-- | Similar to 'genEvaledBlockchainFrom' with a trivial evaluator and
-- state.
--
-- This means that the state hash for will be the same for every block.
genBlockchainFrom
    :: ( IsCrypto c
       , Arbitrary tx
       , Arbitrary s
       , Serialise tx
       , Serialise s
       )
    => Block c tx (Sealed c s)
    -> Gen (Blockchain c tx s)
genBlockchainFrom genBlock =
    genEvaledBlockchainFrom genBlock identityEval ()


-- | 'genBlockchainFrom' with an arbitrary empty genesis block.
genBlockchain
    :: ( IsCrypto c
       , Serialise tx
       , Serialise s
       , Arbitrary tx
       , Arbitrary s
       )
    => Gen (Blockchain c tx s)
genBlockchain = genEvaledBlockchain identityEval ()


data ForkParams = ForkParams
    { forkBranchingFactor :: Natural
    -- ^ Currently not used, but it can determine the /branching factor/
    -- for the fork, i.e. if this fork can generate other forks.
    , forkMaxLength       :: Natural
    -- ^ The maximum size for the generated fork.
    , forkNumber          :: Natural
    -- ^ The number of desired forks to generate.
    -- For example, a @forkProbability@ of 3 will ensure that /at most/
    -- (but there could be less) forks will be generated.
    }

-- | A 'ChainEvent' is an internal type used by the generators to decidec whether
-- or not originate a new fork from a given block.
data ChainEvent =
        DoNotFork
      | ForkChain
      deriving Show

-- | Generatens some chain events in a shuffled, unpredictable order.
genChainEvents :: Int
               -- ^ The size of the input chain.
               -> ForkParams
               -> Gen [ChainEvent]
genChainEvents inputChainSize ForkParams{..} = do
    doFork   <- vectorOf (fromIntegral forkNumber) (pure ForkChain)
    dontFork <- vectorOf (inputChainSize - fromIntegral forkNumber) (pure DoNotFork)
    shuffle (doFork ++ dontFork)


-- | Generates a bunch of chain \"candidates\" to be used in more articulated
-- tests wanting to assess the resilience of fork selection. Here is the
-- general idea: traverse the input blockchain and for each block, with
-- random probability, branch off, creating a fork. This won't be a proper
-- fork, as we will deliberately create orphan chains, where we will need a
-- \"missing link\" in order to hook the orphan chain to the adopted one.
-- Return each and every orphan chain together with the \"missing link\"
-- necessary for the fork-selection algorithm to kick in.
-- Example (ghci session):
--
-- Ok, 134 modules loaded.
-- > let forkParams = ForkParams 0 5 3
-- > (myChain :: Blockchain Text ()) <- generate $ resize 9 (genBlockchainFrom (emptyGenesisBlock epoch))
-- > (orphans :: [(Blockchain Text (), Block Text ())]) <- generate $ genOrphanChainsFrom forkParams myChain
-- > showChainDigest myChain
-- "[EWokaLC (p:1111111) @0ns] <- [AxPcHbF (p:EWokaLC) @48m] <-
--  [76V2FAc (p:AxPcHbF) @1h] <- [YxLsVxJ (p:76V2FAc) @2h] <-
--  [5At4Aku (p:YxLsVxJ) @3h] <- [GACXY9Z (p:5At4Aku) @4h] <-
--  [AEW78LF (p:GACXY9Z) @5h] <- [FJSpSvu (p:AEW78LF) @6h] <-
-- [7zUXruW (p:FJSpSvu) @6h] <- [GGwLhg3 (p:7zUXruW) @7h]"
-- > Prelude.map (bimap showChainDigest showBlockDigest) orphans
-- [
--     (
--       "[9kGRVVf (p:G5jzmbW) @9h] <- [HjaQguf (p:9kGRVVf) @10h] <- [3R1XFCc (p:HjaQguf) @11h] <- [Bm2ETah (p:3R1XFCc) @11h]"
--     ,"[G5jzmbW (p:GGwLhg3) @8h]"
--   )
--   ,(
--       "[JCnXTHy (p:5RsGYYi) @3h] <- [6LPu23A (p:JCnXTHy) @4h] <- [DQRjgLS (p:6LPu23A) @4h] <- [Bdzwgr9 (p:DQRjgLS) @5h]"
--     ,"[5RsGYYi (p:76V2FAc) @2h]"
--   )
--   ,(
--       "[6eX4ctN (p:J1b4MKm) @1h] <- [H5k1K9A (p:6eX4ctN) @2h] <- [3oiWxf5 (p:H5k1K9A) @3h] <- [GTsUC2w (p:3oiWxf5) @4h]"
--     ,"[J1b4MKm (p:EWokaLC) @47m]"
--   )
-- ]
--
-- Note how the system generated 3 forks (as instructed). For the first one,
-- the \"missing link\" is block with hash @G5jzmbW@, which parent is
-- @GGwLhg3@, which is on the main chain.
--
genOrphanChainsFrom
    :: forall c tx s.
       ( Arbitrary tx
       , Arbitrary s
       , Serialise s
       , Serialise tx
       , IsCrypto c
       )
    => ForkParams
    -> Blockchain c tx s
    -- ^ The input (adopted) chain.
    -> Gen [(Blockchain c tx s, Block c tx (Sealed c s))]
    -- ^ A list of potential orphans together with
    -- the missing link that, upon insertion, will
    -- yield a fork.
genOrphanChainsFrom forkParams@ForkParams{..} inputChain = do
    let genesisFirst = Chrono.toOldestFirst
                     . Chrono.reverse
                     . blocks
                     $ inputChain
    events <- genChainEvents (length genesisFirst) forkParams
    go (zip events genesisFirst) [] -- Starts with genesis first
    where
        go :: [(ChainEvent, Block c tx (Sealed c s))]
           -> [(Blockchain c tx s, Block c tx (Sealed c s))]
           -> Gen [(Blockchain c tx s, Block c tx (Sealed c s))]
        go [] !acc = pure acc
        go ((event,x):xs) !acc =
            case event of
              DoNotFork -> go xs acc
              ForkChain -> do
                  forkSize <- choose ( min 3 (fromIntegral forkMaxLength)
                                     , fromIntegral forkMaxLength
                                     )
                  fork <- resize forkSize (genBlockchainFrom x)
                  let (blks, orphanChain) = map (unsafeToBlockchain . reverse)
                                          . splitAt 1
                                          . drop 1 -- drop the tip, which is 'x'.
                                          . Chrono.toOldestFirst
                                          . Chrono.reverse
                                          . blocks
                                          $ fork
                  -- NOTE(adn) Ignoring for now more complicated scenarios
                  -- where we could have a fork of a fork.
                  case blks of
                    [missingBlock] -> go xs ((orphanChain, missingBlock) : acc)
                    _ -> go xs acc


-- | Generate a 'Blockchain' that is valid under
-- 'Nakamoto.validateFull'.
--
-- In particular this means that the difficulty is correctly computed
-- for the blockchain. However, the proof-of-work (i.e. the right
-- nonce) is not computed.
genNakamotoBlockchain
    :: forall c tx. (IsCrypto c, Serialise tx, Arbitrary tx)
    => Gen (Blockchain c tx Nakamoto.PoW)
genNakamotoBlockchain = do
    blockTimestamp <- arbitrary
    genesisSeal <- genPoWSeal
    let genHeader = emptyHeader
            { blockSeal = genesisSeal
            , blockTimestamp
            , blockTargetDifficulty = Nakamoto.minDifficulty
            }
    let genBlock = mkBlock genHeader []
    sized $ \n -> go (genBlock :| []) n
  where
    go :: NonEmpty (Block c tx (Sealed c Nakamoto.PoW))
       -> Int
       -> Gen (Blockchain c tx Nakamoto.PoW)
    go blks 0 =
        pure $ Blockchain blks
    go blks n = do
        blk <- genNakamotoBlockFrom blks
        go (blk <| blks) (n - 1)
