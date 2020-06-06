-- | 

module Connect where

import Cell
import Style

dI :: Int -> Double
dI = fromIntegral

-- Connects an identified Cell2
connection :: Cell2 -> String
connection c = connectWith (style c) c
  where
    connectWith Morphism = connectMorph
    connectWith Transformation = connectMorph
    connectWith Identity = connectIdentity
    connectWith Space = connectSpace

connectIdentity :: Cell2 -> String
connectIdentity c =
    "\\draw ("
    ++ input
    ++ ".center) to [in=180,out=0] ("
    ++ output
    ++ ".center);"
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