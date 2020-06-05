-- | Puts Ids to the nodes.

module Identify where

import Cell


class Identifiable a where
  identify :: String -> a -> a

instance Identifiable Cell1 where
  identify s c = c { id1 = s ++ "c" }

instance Identifiable Cell2 where
  identify s c = c
    { id2     = s ++ "c"
    , source2 = identify (s ++ "s") (source2 c)
    , target2 = identify (s ++ "t") (target2 c)
    }

instance Identifiable Cell3 where
  identify s c = c
    { id3 = s ++ "c"
    , source3 = identify (s ++ "s") (source3 c)
    , target3 = identify (s ++ "t") (target3 c)
    }

instance (Identifiable a) => Identifiable [a] where
  identify s = zipWith k [0..]
    where
      k :: (Identifiable a) => Int -> a -> a
      k n = identify (s ++ show n)
