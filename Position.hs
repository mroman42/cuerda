-- | Two-dimensional positioning of nodes in a diagram.

module Position
  ( positionDiagram2 )
where

import Cell
import Identify
import Draw

positionDiagram2 :: Double -> Diagram2 -> Diagram2
positionDiagram2 d c = zipWith shiftY (map dI [0..]) $ map (positionRack2 d) c

positionRack2 :: Double -> [Cell2] -> [Cell2]
positionRack2 s r = positionNodes $ positionTargets 0.0 (positionSources 0.0 r)
  where
    totalLenM :: Int
    totalLenM = length r

    positionNodes :: [Cell2] -> [Cell2]
    positionNodes = map (headShiftY 0.5) . positionX s

    positionSources :: Double -> [Cell2] -> [Cell2]
    positionSources acc []    = []
    positionSources acc (c:l) =
      -- Position c and shift it to the given accumulator.
      (c { source2 = shiftX acc $ positionX lenC $ source2 c })
      -- Increase the accumulator by the lenght of C and continue.
        : positionSources (acc + lenC) l
      where
        sizeC :: Int
        sizeC = length (source2 c)
        stepS :: Double
        stepS = s / (dI totalLenS * 2.0)
        lenC :: Double
        lenC = (dI sizeC * 2.0) * stepS
        totalLenS :: Int
        totalLenS = length (concatMap source2 r)

    positionTargets :: Double -> [Cell2] -> [Cell2]
    positionTargets acc [] = []
    positionTargets acc (c:l) =
      (c { target2 = shiftY 1.0 $ shiftX acc $ positionX lenC $ target2 c })
        : positionTargets (acc + lenC) l
      where
        sizeC :: Int
        sizeC = length (target2 c)
        stepT :: Double
        stepT = s / (dI totalLenT * 2.0)
        lenC :: Double
        lenC = (dI sizeC * 2.0) * stepT
        totalLenT :: Int
        totalLenT = length (concatMap target2 r)

    -- | Positions a list of things, relative to height. Given a height h and a list
    -- of positionable things; it divides the height by the length of the list.
    positionX :: (Positioned a) => Double -> [a] -> [a]
    positionX h l = zipWith pos [0..] l
      where
        pos :: (Positioned a) => Int -> a -> a
        pos n = setX (h / (dI (length l) * 2.0) * (dI n * 2.0 + 1.0))

dI :: Int -> Double
dI = fromIntegral
