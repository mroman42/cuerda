-- | 

module Draw where

import Cell
import Style

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
    ++ "\\coordinate ("
    ++ id1 c
    ++ ") at ("
    ++ show (getY c)
    ++ ", "
    ++ show (getX c)
    ++ ") {};"

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
