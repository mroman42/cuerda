module Connect
  ( connection
  , connections
  , connectionsRack3
  ) where

import Cell
import Style

dI :: Int -> Double
dI = fromIntegral

-- Connects an identified Cell of dimension 2, a different connection needs to
-- be declared for every style.  Note how we assume the cells are identified.
connection :: Cell2 -> String
connection c = connectWith (style c) c
  where
    connectWith Morphism = connectMorph
    connectWith Transformation = connectMorph
    connectWith Identity = connectIdentity
    connectWith Space = connectSpace

connectIdentity :: Cell2 -> String
connectIdentity c =
  if (length (source2 c) > 0 && length (target2 c) > 0) then
    "\\draw ("
    ++ input
    ++ ".center) to [in=180,out=0] ("
    ++ output
    ++ ".center);"
   else
    ""
  where
    input = id1 $ head (source2 c)
    output = id1 $ head (target2 c)

connectSpace :: Cell2 -> String
connectSpace c = ""

connectMorph :: Cell2 -> String
connectMorph c = unlines $ concat
  [ zipWith kS [0..] (source2 c)
  , zipWith kT [0..] (target2 c)
  ]
  where
    lenS :: Double
    lenS = dI $ length (source2 c)
    lenT :: Double
    lenT = dI $ length (target2 c)

    kS :: Int -> Cell1 -> String
    kS n p =
      "\\draw ("
      ++ id1 p
      ++ ".center) to [out=0, in="
      ++ show (angleS lenS (dI n))
      ++ "] ("
      ++ id2 c
      ++ ".center);"

    kT :: Int -> Cell1 -> String
    kT n p =
      "\\draw ("
      ++ id1 p
      ++ ".center) to [out=180, in="
      ++ show (angleT lenT (dI n))
      ++ "] ("
      ++ id2 c
      ++ ".center);"

    angleS :: Double -> Double -> Double
    angleS n m = ((m+1) / (n+1) * (-180.0)) + 270.0
    angleT :: Double -> Double -> Double
    angleT n m = ((m+1) / (n+1) * (180.0)) - 90.0

connections :: Diagram2 -> String
connections = unlines . concat . map (map connection)

-- Connects an identified 3 cell.
connection3 :: Cell3 -> String
connection3 c = connectWith (style3 c) c
  where
    connectWith Morphism = connect3Transformation
    connectWith Transformation = connect3Transformation
    connectWith Identity = connect3Transformation
    connectWith Space = connect3Empty

connect3Transformation :: Cell3 -> String
connect3Transformation c = unlines $
    map kS (concat (source3 c)) ++
    map kT (concat (target3 c))
  where
    kS :: Cell2 -> String
    kS p = if (style p == Morphism) then
             ("\\draw [red!30] ("
              ++ id2 p
              ++ ".center) to [out=90,in=-90] ("
              ++ id3 c
              ++ ".center);")
           else ""

    kT :: Cell2 -> String
    kT p = if (style p == Morphism) then
             ("\\draw [red!30] ("
              ++ id2 p
              ++ ".center) to [out=-90,in=90] ("
              ++ id3 c
              ++ ".center);")
           else ""

connect3Empty :: Cell3 -> String
connect3Empty _ = ""

connectionsRack3 :: [[Cell3]] -> String
connectionsRack3 = unlines . concat . (map (map connection3))
