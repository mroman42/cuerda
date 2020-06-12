-- | Declares multiple styles for drawing morphisms.

module Style where

data Style =
  Morphism | Identity | Space | Transformation | Empty
  deriving (Eq)
