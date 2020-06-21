module Connect
  ( connection
  , connections
  , connection3
  ) where

import Cell
import Style

dI :: Int -> Double
dI = fromIntegral

-- | Connects an identified Cell of dimension 2, a different connection needs to
-- be declared for every style.  Note how we assume the cells are identified.
connection :: Cell2 -> String
connection c = connectWith (style c) c
  where
    connectWith Morphism = connectMorph
    connectWith Transformation = connectMorph
    connectWith Identity = connectIdentity
    connectWith Anonymous = connectIdentity
    connectWith Space = connectSpace

-- connectIdentity :: Cell2 -> String
-- connectIdentity c =
--   if (length (source2 c) > 0 && length (target2 c) > 0) then
--     "\\draw ("
--     ++ input
--     ++ ".center) to [in=180,out=0] ("
--     ++ output
--     ++ ".center);"
--    else
--     ""
--   where
--     input = id1 $ head (source2 c)
--     output = id1 $ head (target2 c)

connectIdentity :: Cell2 -> String
connectIdentity c = unlines $
  zipWith connectBoth (source2 c) (target2 c)
  where
    input = id1 $ head (source2 c)
    output = id1 $ head (target2 c)
    connectBoth a b = concat
      [ "\\draw ("++id1 a++".center) to [in=180,out=0] ("
      ++id1 b++".center);"]
    

connectSpace :: Cell2 -> String
connectSpace c = ""

connectMorph :: Cell2 -> String
connectMorph c = unlines $ concat
  [ zipWith kS [0..] (source2 c)
  , zipWith kT [0..] (target2 c)
  ]
  where
    lenS :: Int
    lenS = length (source2 c)
    lenT :: Int
    lenT = length (target2 c)

    kS :: Int -> Cell1 -> String
    kS n p = if (style1 p == Invisible) then "" else concat
     ["\\draw (",id1 p,".center) to [out=0, in=",show (angleS lenS n),"] (",id2 c,".center);"]

    kT :: Int -> Cell1 -> String
    kT n p = if (style1 p == Invisible) then "" else concat
       ["\\draw (",id1 p,") to [out=180, in=",show (angleT lenT n),"] (",id2 c,");"]

    -- Rounded version, as by E. Di Lavore.
    angleS :: Int -> Int -> Double
    angleS n m = 180.0/((dI n)-1)*(dI m) + 90.0

    -- Previous version
    angleS' :: Int -> Int -> Double
    angleS' n m = ((dI m+1) / (dI n+1) * (-180.0)) + 270.0

    angleT :: Int -> Int -> Double
    angleT n m = (((dI m)+1) / ((dI n)+1) * (180.0)) - 90.0

connections :: Diagram2 -> String
connections = unlines . concat . map (map connection)


-- Connects an identified 3 cell.
connection3 :: Cell3 -> String
connection3 c = connectWith (style3 c) c
  where
    connectWith Morphism = connect3Transformation
    connectWith Transformation = connect3Transformation
    connectWith Identity = connect3Identity
    connectWith Anonymous = connect3Transformation
    connectWith Space = connect3Empty

connect3Identity :: Cell3 -> String
connect3Identity c = unlines $
    zipWith connectBoth
      (filter realNodes (concat (source3 c)))
      (filter realNodes (concat (target3 c)))
  where
    connectBoth a b = concat
      [ "\\draw [red!30] ("++id2 a++".center) to [in=270,out=90] ("
      ++id2 b++".center);"]
    -- Only the nodes that are drawn
    realNodes x = (style x == Morphism)

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
