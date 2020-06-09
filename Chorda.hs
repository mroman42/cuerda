-- |

module Chorda
  ( module Cell
  , module Identify
  , module Connect
  , module Style
  , module Draw
  , module Print
  , module Chorda )
where

import Cell
import Identify
import Connect
import Style
import Draw
import Print

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

-- 3D positioning is easier because we are going to actually use scopes anyway.
-- We should just map the positioning to every layer and then position the core nodes.
drawDiagram2 :: Double -> Diagram2 -> String
drawDiagram2 d c =
  -- Draw the nodes
  unlines (map draw (positionDiagram2 d c))
  -- Draw the connections
  ++ connections c
  -- And draw again the nodes over them
  ++ unlines (map draw (positionDiagram2 d c))

drawSomeDiagrams :: Double -> [Diagram2] -> String
drawSomeDiagrams d l = unlines
  $ zipWith scope [0..]
  $ map (drawDiagram2 d . shiftBySize) l
  where
    scope :: Int -> String -> String
    scope n s =
      "\\begin{scope}[tilted,yshift="
      ++ show (dI n * 0.8)
      ++ "cm]\n"
      ++ s
      ++ "\n"
      ++ "\\end{scope}"

    shiftBySize :: Diagram2 -> Diagram2
    shiftBySize c = shiftY (-dI (length c) / 2.0) c


-- Drawing 3d diagrams.
-- drawDiagram3 :: Double -> Diagram3 -> String
-- drawDiagram3 d [[[c]]] = drawDiagram2 d (source3 c)

-- Drawing 3d diagrams.
drawRack3 :: Double -> Double -> [[Cell3]] -> String
drawRack3 d h c =
       scope 0 (drawDiagram2 d (shiftBySize theSource))
    ++ scope 0.5 (drawDiagram2 d (shiftBySize theMiddle))
    ++ scope 1 (drawDiagram2 d (shiftBySize theTarget))
    ++ fili [theSource, theTarget]
  where
    -- Construct the source by aggregating sources.
    theSource :: Diagram2
    theSource = foldr1 seq2 $ map (foldr1 par2) $ map (map source3) c
    
    -- Construct the target by aggregating targets.
    theTarget :: Diagram2
    theTarget = foldr1 seq2 $ map (foldr1 par2) $ map (map target3) c

    -- Form the middle diagram.
    demote :: Cell3 -> Cell2
    demote c = Cell2
      { name2 = name3 c
      , source2 = []
      , target2 = []
      , id2 = id3 c
      , xpos2 = 0.0
      , ypos2 = 0.0
      , style = style3 c
      }

    theMiddle :: Diagram2
    theMiddle = map (map demote) c

    -- Shift by size
    shiftBySize :: Diagram2 -> Diagram2
    shiftBySize c = shiftY (-dI (length c) / 2.0) c

    -- Scope a diagram
    scope :: Double -> String -> String
    scope n s =
      "\\begin{scope}[tilted,yshift="
      ++ show (n * h)
      ++ "cm]\n"
      ++ s
      ++ "\n"
      ++ "\\end{scope}"
    

type Connections = [((Int,Int,Int),(Int,Int,Int))]


fili :: [Diagram2] -> String
fili c =
  unlines (zipWith connectIds o i)
  ++ "\n" ++
  unlines (zipWith connectIds o2 i2)
  where
    i = map id1 $ (concatMap source2) (head $ head c)
    o = map id1 $ (concatMap source2) (head $ last c)

    i2 = map id1 $ (concatMap target2) (last $ head c)
    o2 = map id1 $ (concatMap target2) (last $ last c)



extraConnections :: [Diagram2] -> [((Int,Int,Int),(Int,Int,Int))] -> String
extraConnections c = unlines . map (connect c)
  where
    connect :: [Diagram2] -> ((Int,Int,Int),(Int,Int,Int)) -> String
    connect c ((x1,x2,x3),(y1,y2,y3)) = connectIds firstId secondId
      where
        firstId = id2 (c !! x1 !! x2 !! x3) :: String
        secondId = id2 (c !! y1 !! y2 !! y3) :: String

connectIds :: Id -> Id -> String
connectIds firstId secondId =
      "\\draw [red!30] ("
      ++ firstId
      ++ ".center) to [out=-90,in=90] ("
      ++ secondId
      ++ ".center);"


data Diagram3d = Diagram3d
  { commandName :: String
  , cells :: [[Cell3]]
  }

mkDiagram3D :: String -> [[Cell3]] -> Diagram3d
mkDiagram3D s c = Diagram3d s (identify "i" c)

instance Show Diagram3d where
  show c = unlines
    [ "\\newcommand{\\" ++ commandName c ++ "}{"
    , "\\begin{tikzpicture}[cordadiagram]"
    , drawRack3 2 2 (cells c)
    , connectionsRack3 (cells c)
    , drawRack3 2 2 (cells c)
    , "\\end{tikzpicture}}" ]


-- \begin{scope}[every path/.style={-,out=0,in=180}]
--  \draw (prodOne-o2) to (prodTwo-i2);
--  \draw (prodOne-o1) to (prodTwo-i1);
-- \end{scope}

