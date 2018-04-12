module Oscoin.View where

import           Oscoin.Prelude
import qualified Oscoin.Consensus.Class as Consensus
import qualified Oscoin.Org.Transaction as Org
import qualified Oscoin.Org             as Org
import           Oscoin.State.Tree (Tree, Path, Val)
import qualified Oscoin.State.Tree as Tree

import           Control.Monad.State (State, get, modify)

type T = Tree Path Val

newtype View a = View (State T a)
    deriving ( Functor
             , Applicative
             , Monad
             , MonadState T
             )

instance Consensus.Context View where
    type State View = T
    type Key View = Path

    get _k = undefined
    set _k _v = undefined
    del _k = undefined

instance Consensus.View View where
    type Transaction View = Org.Tx
    type BlockHeader View = ()

    apply Nothing txs = do
        for_ txs $ \tx ->
            case tx of
                Org.SetTx org k v ->
                    modify $ Tree.set (Org.mkOrgDataPath org [k]) v
                _ ->
                    notImplemented
        get
    apply _ _ =
        notImplemented
