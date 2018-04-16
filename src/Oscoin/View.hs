module Oscoin.View where

import           Oscoin.Prelude
import qualified Oscoin.Consensus.Class as Consensus
import qualified Oscoin.Account.Transaction as Account
import qualified Oscoin.Account             as Account
import           Oscoin.State.Tree (Tree, Path, Val)
import qualified Oscoin.State.Tree as Tree

import           Control.Monad.State (State, modify)

type T = Tree Path Val

newtype View a = View (State T a)
    deriving ( Functor
             , Applicative
             , Monad
             , MonadState T
             )

instance Consensus.Context View where
    type T View = T
    type Key View = Path

    get _k = undefined
    set _k _v = undefined
    del _k = undefined

instance Consensus.View View where
    type Transaction View = Account.Tx
    type BlockHeader View = ()

    apply Nothing txs =
        for_ txs $ \tx ->
            case tx of
                Account.SetTx acc k v ->
                    modify $ Tree.set (Account.mkAccDataPath acc [k]) v
                _ ->
                    notImplemented
    apply _ _ =
        notImplemented
