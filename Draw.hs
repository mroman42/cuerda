-- | 

module Draw where

import Cell
import Position
import Style
import Connect

drawDiagram2 :: Double -> Diagram2 -> String
drawDiagram2 d c =
  -- Draw only the coordinates
  drawCoordinatesDiagram2 (positionDiagram2 d c)
  -- Draw the connections
  ++ drawConnectionsDiagram2 c
  -- And draw again the nodes over them
  ++ drawNodesDiagram2 (positionDiagram2 d c)

drawCoordinatesCell1 :: Cell1 -> String
drawCoordinatesCell1 c = concat
  [ "\\coordinate (",id1 c, ") at (",show (getY c),", ",show (getX c),"){};" ]

drawCoordinatesFullCell2 :: Cell2 -> String
drawCoordinatesFullCell2 c = unlines
  [ drawCoordinatesCoreCell2 c
  , concatMap drawCoordinatesCell1 (source2 c)
  , concatMap drawCoordinatesCell1 (target2 c)
  ]
  
drawCoordinatesCoreCell2 :: Cell2 -> String
drawCoordinatesCoreCell2 c = concat
  [ "\\coordinate (",id2 c, ") at (",show (getY c),", ",show (getX c),"){};" ]

drawCoordinatesDiagram2 :: PositionedDiagram2 -> String
drawCoordinatesDiagram2 = unlines . map (concatMap drawCoordinatesFullCell2)

drawConnectionsDiagram2 :: Diagram2 -> String
drawConnectionsDiagram2 = unlines . concat . map (map connection)

drawNodesDiagram2 :: Diagram2 -> String
drawNodesDiagram2 = unlines . map draw

drawConnectionsAndNodesDiagram2 :: Diagram2 -> String
drawConnectionsAndNodesDiagram2 c = concat
  [ drawConnectionsDiagram2 c
  , drawNodesDiagram2 c
  ]

class Draw a where
  draw :: a -> String

instance Draw Cell1 where
  draw c =
    -- Grey hint, optionally
    "\\node ("
    ++ "none"
    ++ ") [hint] at ("
    ++ show (getY c)
    ++ ", "
    ++ show (getX c)
    ++ ") {};\n"

    -- Real Coordinate
    ++ drawCoordinatesCell1 c

instance (Draw a) => Draw [a] where
  draw = unlines . map draw

instance Draw Cell2 where
  draw c = unlines [ drawCore c , draw (source2 c) , draw (target2 c) ]
   where
    drawCore :: Cell2 -> String
    drawCore c = drawWith (style c) c
       where
         drawWith Morphism = drawMorph
         drawWith Transformation = drawTransf
         drawWith Identity = drawIdentity
         drawWith Space = drawSpace

    drawSpace :: Cell2 -> String
    drawSpace c = ""

    drawMorph :: Cell2 -> String
    drawMorph c = "\\node ("
      ++ id2 c
      ++ ") [morphism] at ("
      ++ show (getY c)
      ++ ", "
      ++ show (getX c)
      ++ ") {$"
      ++ name2 c
      ++ "$};"

    drawTransf :: Cell2 -> String
    drawTransf c = "\\node ("
      ++ id2 c
      ++ ") [transformation] at ("
      ++ show (getY c)
      ++ ", "
      ++ show (getX c)
      ++ ") {$"
      ++ name2 c
      ++ "$};"

    drawIdentity :: Cell2 -> String
    drawIdentity c =
      "\\coordinate ("
      ++ id2 c
      ++ ") at ("
      ++ show (getY c)
      ++ ", "
      ++ show (getX c)
      ++ ") {};"

-- 3D positioning is easier because we are going to actually use scopes anyway.
-- We should just map the positioning to every layer and then position the core nodes.

drawConnectionsRack3 :: [[Cell3]] -> String
drawConnectionsRack3 = unlines . concat . (map (map connection3))

-- drawCoordinatesRack3 :: [[Cell3]] -> String
-- drawCoordinatesRack3 =  


-- Drawing 3d diagrams.
drawRack3 :: Double -> Double -> [[Cell3]] -> String
drawRack3 d h c = concat
    [ scope 0 (drawCoordinatesDiagram2 (positionDiagram2 d (shiftBySize theSource)))
    , scope 1 (drawCoordinatesDiagram2 (positionDiagram2 d (shiftBySize theTarget)))
    , fili [theSource, theTarget]
    , scope 1 (drawConnectionsAndNodesDiagram2 (positionDiagram2 d (shiftBySize theTarget)))
    , scope 0.5 (drawConnectionsAndNodesDiagram2 (positionDiagram2 d (shiftBySize theMiddle)))
    , scope 0 (drawConnectionsAndNodesDiagram2 (positionDiagram2 d (shiftBySize theSource)))
    ]
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

    scope :: Double -> String -> String
    scope n s = concat
      [ "\\begin{scope}[tilted,yshift=",show(n*h),"cm]\n",s,"\n\\end{scope}" ]

-- Scope a diagram.
scope :: Double -> Double -> String -> String
scope h n s = concat
  [ "\\begin{scope}[tilted,yshift=",show(n*h),"cm]\n",s,"\n\\end{scope}" ]

drawDiagramIn3d :: Double -> Double -> [[[Cell3]]] -> String
drawDiagramIn3d d h c = unlines $ reverse $ zipWith drawInScope [0..] c
  where
    drawInScope :: Int -> [[Cell3]] -> String
    drawInScope n c = scope n $ unlines
      [ drawRack3 d h c
      , drawConnectionsRack3 c
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


dI :: Int -> Double
dI = fromIntegral
