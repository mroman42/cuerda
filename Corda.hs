module Corda
  ( module Cell
  , module Identify
  , module Connect
  , module Style
  , module Draw
  , module Print
  , module Position
  , module Corda )
where

import Cell
import Identify
import Connect
import Style
import Draw
import Position
import Print




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
    ++ scope 0 (drawDiagram2 d (shiftBySize theSource))
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

    -- Final touches
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

drawDiagramIn3d :: Double -> Double -> [[[Cell3]]] -> String
drawDiagramIn3d d h c = unlines $ zipWith drawInScope [0..] c
  where
    drawInScope :: Int -> [[Cell3]] -> String
    drawInScope n c = scope n $ unlines
      [ drawRack3 d h c
      , connectionsRack3 c
      , drawRack3 d h c
      ]

    -- Scope a diagram
    scope :: Int -> String -> String
    scope n s =
      "\\begin{scope}[yshift="
      ++ show (dI n * 1 * h)
      ++ "cm]\n"
      ++ s
      ++ "\n"
      ++ "\\end{scope}"

connectIds :: Id -> Id -> String
connectIds firstId secondId =
      "\\draw [borders] ("
      ++ firstId
      ++ ".center) to [out=-90,in=90] ("
      ++ secondId
      ++ ".center);"


data Diagram3d = Diagram3d
  { commandName3d :: String
  , cells3d :: [[[Cell3]]]
  }

data Diagram2d = Diagram2d
  { commandName2d :: String
  , cells2d :: [[Cell2]]
  }

latex3D :: [[[Cell3]]] -> IO ()
latex3D c = putStr $ unlines
  [ "\\begin{tikzpicture}[cordadiagram]"
  , drawDiagramIn3d 2 2.5 (identify "i" c)
  , "\\end{tikzpicture}" ]

latex2D :: [[Cell2]] -> IO ()
latex2D c = putStr $ unlines
  [ "\\begin{tikzpicture}[cordadiagram]"
  , drawDiagram2 2 (identify "i" c)
  , "\\end{tikzpicture}" ]


mkDiagram3D :: String -> [[[Cell3]]] -> String
mkDiagram3D s c = show (Diagram3d s (identify "i" c))

instance Show Diagram3d where
  show c = unlines
    [ "\\newcommand{\\" ++ commandName3d c ++ "}{"
    , "\\begin{tikzpicture}[cordadiagram]"
    , drawDiagramIn3d 2 2.5 (cells3d c)
    , "\\end{tikzpicture}}" ]

mkDiagram2D :: String -> [[Cell2]] -> String
mkDiagram2D s c = show (Diagram2d s (identify "i" c))

instance Show Diagram2d where
  show c = unlines
    [ "\\newcommand{\\" ++ commandName2d c ++ "}{"
    , "\\begin{tikzpicture}[cordadiagram]"
    , drawDiagram2 2 (cells2d c)
    , "\\end{tikzpicture}}"
    ]

dI :: Int -> Double
dI = fromIntegral
