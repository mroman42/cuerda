-- | 

module Draw where

import Cell
import Position
import Style
import Connect
import Settings

drawDiagram2 :: Settings -> Double -> Diagram2 -> String
drawDiagram2 settings d c =
  -- Draw only the coordinates
  drawCoordinatesDiagram2 (positionDiagram2 d c)
  -- Draw the connections
  ++ drawConnectionsDiagram2 c
  -- And draw again the nodes over them
  ++ drawNodesDiagram2 settings (positionDiagram2 d c)

drawCoordinatesCell1 :: Cell1 -> String
drawCoordinatesCell1 c = concat
  [ "\\coordinate (",id1 c,") at (",show (getY c),", ",show (getX c),"){};" ]

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

drawNodesDiagram2 :: Settings -> Diagram2 -> String
drawNodesDiagram2 settings = unlines . map (unlines . map (drawComplete2Cell settings))

drawConnectionsAndNodesDiagram2 :: Settings -> Diagram2 -> String
drawConnectionsAndNodesDiagram2 settings c = concat
  [ drawConnectionsDiagram2 c
  , drawNodesDiagram2 settings c
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

-- instance Draw Cell2 where
--   draw c = unlines [ drawCore2Cell defaultSettings c , draw (source2 c) , draw (target2 c) ]

drawComplete2Cell :: Settings -> Cell2 -> String
drawComplete2Cell settings c = unlines [ drawCore2Cell settings c , draw (source2 c) , draw (target2 c) ]

-- Draws the core of a 2-cell
drawCore2Cell :: Settings -> Cell2 -> String
drawCore2Cell settings c = drawWith (style c) c
   where
    drawWith Morphism = drawMorph settings
    drawWith Transformation = drawTransf settings
    drawWith Identity = drawIdentity
    drawWith Anonymous = drawIdentity
    drawWith Space = drawSpace

    drawSpace :: Cell2 -> String
    drawSpace c = ""

    drawMorph :: Settings -> Cell2 -> String
    drawMorph settings c = concat
      ["\\node (",id2 c,") [",nodeSettings2 c,"] at (",show (getY c),", ",show (getX c),") {$",name2 c,"$};"]

    drawTransf :: Settings -> Cell2 -> String
    drawTransf settings c = concat
      ["\\node (",id2 c,")[",transformationStyle settings,"] at (",show (getY c),", ",show (getX c),") {$",name2 c,"$};"]

    drawIdentity :: Cell2 -> String
    drawIdentity c = concat
      ["\\coordinate (",id2 c,") at (",show (getY c),", ",show (getX c),") {};"]


-- 3D positioning is easier because we are going to actually use scopes anyway.
-- We should just map the positioning to every layer and then position the core nodes.
drawConnectionsRack3 :: Settings -> [[Cell3]] -> String
drawConnectionsRack3 settings = unlines . concat . (map (map (connection3 settings)))


-- Drawing 3d diagrams.
drawRack3 :: Settings -> Double -> Double -> [[Cell3]] -> String
drawRack3 settings d h c = concat
    [ scope 0 (drawCoordinatesDiagram2 (positionDiagram2 d (shiftBySize theSource)))
    , scope 1 (drawCoordinatesDiagram2 (positionDiagram2 d (shiftBySize theTarget)))
    , fili [theSource, theTarget]
    , scope 1 (drawConnectionsAndNodesDiagram2 settings (positionDiagram2 d (shiftBySize theTarget)))
    , scope 0.5 (drawConnectionsAndNodesDiagram2 settings (positionDiagram2 d (shiftBySize theMiddle)))
    , scope 0 (drawConnectionsAndNodesDiagram2 settings (positionDiagram2 d (shiftBySize theSource)))
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
      , nodeSettings2 = defaultMorphismStyle
      }

    theMiddle :: Diagram2
    theMiddle = map (map demote) c

    -- Shift by size
    shiftBySize :: Diagram2 -> Diagram2
    shiftBySize c = shiftY (-dI (length c) / 2.0) c

    -- Final touches, exterior borders.
    fili :: [Diagram2] -> String
    fili c =
      unlines (zipWith (connectIds (borderLeftStyle settings)) o  i) ++ "\n" ++
      unlines (zipWith (connectIds (borderRightStyle settings)) o2 i2)
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

drawDiagramIn3d :: Settings -> Double -> Double -> [[[Cell3]]] -> String
drawDiagramIn3d settings d h c = unlines $ reverse $ zipWith drawInScope [0..] c
  where
    drawInScope :: Int -> [[Cell3]] -> String
    drawInScope n c = scope n $ unlines
      [ drawRack3 settings d h c
      , drawConnectionsRack3 settings c
      , drawRack3 settings d h c
      ]

    -- Enclose diagrams in scopes.
    scope :: Int -> String -> String
    scope n s = concat
      ["\\begin{scope}[yshift=",show (dI n * 1 * h),"cm]\n", s,"\n\\end{scope}"]

connectIds :: String -> Id -> Id -> String
connectIds borderSettings firstId secondId = concat
  ["\\draw [",borderSettings,"] (",firstId,".center) to [out=-90,in=90] (",secondId,".center);"]


dI :: Int -> Double
dI = fromIntegral
