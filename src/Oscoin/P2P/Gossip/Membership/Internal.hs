module Oscoin.P2P.Gossip.Membership.Internal
    ( Peers
    , active
    , passive
    , activeL
    , passiveL

    , TimeToLive
    , isExpired
    ) where

import           Oscoin.Prelude

import           Lens.Micro (Lens', lens)

data Peers n c = Peers
    { active  :: Map n c
    , passive :: Set n
    }

instance Ord n => Semigroup (Peers n c) where
    a <> b = Peers
        { active  = active  a <> active  b
        , passive = passive a <> passive b
        }

instance Ord n => Monoid (Peers n c) where
    mempty  = Peers mempty mempty
    mappend = (<>)

activeL :: Lens' (Peers n c) (Map n c)
activeL = lens active (\s a -> s { active = a })
{-# INLINE activeL #-}

passiveL :: Lens' (Peers n c) (Set n)
passiveL = lens passive (\s a -> s { passive = a })
{-# INLINE passiveL #-}

-- | The time-to-live of a random walk.
--
-- \"Forward Join\" and \"Shuffle\" RPCs are propagated using a random walk,
-- where each hop decrements the 'TimeToLive' until it 'isExpired', at which
-- point the respective operation is applied to the current node's state. We
-- retain the terminology, even though no time component is involved.
newtype TimeToLive = TimeToLive { fromTimeToLive :: Word8 }
    deriving (Eq, Num, Bounded, Ord)

instance Enum TimeToLive where
    succ ttl | ttl < maxBound = TimeToLive . succ . fromTimeToLive $ ttl
             | otherwise      = ttl

    pred ttl | ttl > minBound = TimeToLive . pred . fromTimeToLive $ ttl
             | otherwise      = ttl

    toEnum   = TimeToLive . fromIntegral
    fromEnum = fromIntegral . fromTimeToLive

isExpired :: TimeToLive -> Bool
isExpired (TimeToLive t) = t == 0

