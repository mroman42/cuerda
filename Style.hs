-- | Declares multiple styles for drawing morphisms.

module Style where

data Style =
  Morphism | Identity | Space | Transformation | Empty | Anonymous
  deriving (Eq)

data Style1
  = Normal -- A normal wire
  | Invisible -- An invisible wire
  | Custom String -- A custom wire
  deriving (Eq)
