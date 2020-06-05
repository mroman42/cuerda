-- |

module Chorda where

import Cell
import Identify
import Connect
import Style
import Draw

-- dI :: Int -> Double
-- dI = fromIntegral

-- Demoting a diagram
-- demote2to1 :: Cell2 -> Cell1
-- demote2to1 c = Cell1
--   { name1 = name2 c
--   , id1 = id2 c
--   , xpos1 = xpos2 c
--   , ypos1 = ypos2 c
--   }

-- Positioning a list of things, relative to a height.
positionX :: (Positioned a) => Double -> [a] -> [a]
positionX d l = zipWith pos [0..] l
  where
    pos :: (Positioned a) => Int -> a -> a
    pos n = setX (d / (dI (length l) * 2.0) * (dI n * 2.0 + 1.0))

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

positionDiagram2 :: Double -> Diagram2 -> Diagram2
positionDiagram2 d c = zipWith shiftY (map dI [0..]) $ map (positionRack2 d) c

drawDiagram2 :: Double -> Diagram2 -> String
drawDiagram2 d = unlines . map draw . positionDiagram2 d


----
-- Examples
----
example :: [Cell2]
example = [f,g,h]
  where
    a = cell1 "a"
    b = cell1 "b"
    f = cell2 "f" [a,b] [b]
    d = cell2 "d" [a,b] []
    g = cell2 "g" [a] [b]
    h = cell2 "h" [a] [a,b,b]

example3 :: [[Cell2]]
example3 = [[h],[f,idt a],[d]]
  where
    a = cell1 "a"
    b = cell1 "b"
    f = cell2 "f" [a,b] [b]
    d = cell2 "d" [a,b] []
    g = cell2 "g" [a] [b]
    h = cell2 "h" [a] [a,b,b]

example4 :: [[Cell2]]
example4 = [[ol],[t,idt a],[or]]
  where
    a = cell1 "a"
    ol = cell2 "\\otimes" [a] [a,a]
    t = cell2 "T" [a] [a]
    or = cell2 "\\otimes" [a,a] [a]

example5 :: [[Cell2]]
example5 = [[ol],[ol,idt a]]
  where
    a = cell1 "a"
    ol = cell2 "\\otimes" [a] [a,a]
    t = cell2 "T" [a] [a]
    or = cell2 "\\otimes" [a,a] [a]

example2 :: [Cell1]
example2 = [a,a,a]
  where
    a = cell1 "a"

main :: IO ()
main = do
  readFile "latexHeader.tex" >>= putStrLn
  let ex = identify "u" example5
  putStrLn $ drawDiagram2 1.5 ex
  putStrLn $ connections      ex
  putStrLn $ drawDiagram2 1.5 ex
  -- putStrLn $ draw (positionRack2 6.0 example)
  readFile "latexFooter.tex" >>= putStrLn
