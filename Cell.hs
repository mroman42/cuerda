-- |
{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}

module Cell where

type Label = String
type Id = String

-- Cells
data Cell1 = Cell1
  { name1 :: Label
  , id1 :: Id
  , xpos1 :: Double
  , ypos1 :: Double
  }

cell1 :: Label -> Cell1
cell1 l = Cell1 l "none" 0.0 0.0


type Diagram1 = [Cell1]

data Cell2 = Cell2
  { name2 :: Label
  , source2 :: Diagram1
  , target2 :: Diagram1
  , id2 :: Id
  , xpos2 :: Double
  , ypos2 :: Double
  }

cell2 :: Label -> [Cell1] -> [Cell1] -> Cell2
cell2 l s t = Cell2 l s t "none" 0.0 0.0

type Diagram2 = [[Cell2]]

data Cell3 = Cell3
  { name3 :: Label
  , source3 :: Diagram2
  , target3 :: Diagram2
  , id3 :: Id
  , xpos3 :: Double
  , ypos3 :: Double
  }

type Diagram3 = [[[Cell3]]]

cell3 :: Label -> [[Cell2]] -> [[Cell2]] -> Cell3
cell3 l s t = Cell3 l s t "none" 0.0 0.0



class Shiftable a where
  shiftX :: Double -> a -> a
  shiftY :: Double -> a -> a

class HeadShiftable a where
  headShiftX :: Double -> a -> a
  headShiftY :: Double -> a -> a

class Positioned a where
  getX :: a -> Double
  getY :: a -> Double
  setX :: Double -> a -> a
  setY :: Double -> a -> a

instance (Shiftable a) => Shiftable [a] where
  shiftX x = map (shiftX x)
  shiftY y = map (shiftY y)

instance Shiftable Cell1 where
  shiftX x c = c { xpos1 = xpos1 c + x }
  shiftY y c = c { ypos1 = ypos1 c + y }

instance HeadShiftable Cell2 where
  headShiftX x c = c { xpos2 = xpos2 c + x }
  headShiftY y c = c { ypos2 = ypos2 c + y }

instance Shiftable Cell2 where
  shiftX x c =
    -- Not only shifts the head node, but also all the others.
    headShiftX x $ c
      { source2 = shiftX x (source2 c)
      , target2 = shiftX x (target2 c)
      }
  shiftY y c =
    -- Not only shifts the head node, but also all the others.
    headShiftY y $ c
      { source2 = shiftY y (source2 c)
      , target2 = shiftY y (target2 c)
      }


instance Positioned Cell1 where
  getX = xpos1
  getY = ypos1
  setX x c = c { xpos1 = x }
  setY y c = c { ypos1 = y }

instance Positioned Cell2 where
  getX = xpos2
  getY = ypos2
  setX x c = c { xpos2 = x }
  setY y c = c { ypos2 = y }
