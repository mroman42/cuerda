-- |

module Chorda where

type Label = String

-- Cells
data Cell1 = Cell1
  { name1 :: Label
  }

type Diagram1 = [Cell1]

data Cell2 = Cell2
  { name2 :: Label
  , source2 :: Diagram1
  , target2 :: Diagram1
  }

type Diagram2 = [[Cell2]]

data Cell3 = Cell3
  { name3 :: Label
  , source3 :: Diagram2
  , target3 :: Diagram2
  }

type Diagram3 = [[[Cell3]]]


-- Cell constructors
cell1 :: Label -> Cell1
cell1 = Cell1

cell2 :: Label -> [Cell1] -> [Cell1] -> Cell2
cell2 = Cell2

cell3 :: Label -> [[Cell2]] -> [[Cell2]] -> Cell3
cell3 = Cell3



-- Drawing a rack of morphisms in a given space.
drawRack2 :: [Cell2] -> Double -> String
drawRack2 l d =
  unlines $ map (format . position) [0..lsi]
  where
    position :: Int -> Double
    position n = d / (lsd * 2.0 + 1.0) * ((fromIntegral n + 1.0) * 2.0)

    lsi :: Int
    lsi = length $ concatMap source2 l
    lsd :: Double
    lsd = fromIntegral lsi

    totalLengthTarget :: [Cell2] -> Double
    totalLengthTarget l = fromIntegral $ length $ concatMap target2 l
    totalLengthSource :: [Cell2] -> Double
    totalLengthSource l = fromIntegral $ length $ concatMap source2 l

    format :: Double -> String
    format n = "\\node [morphism] at (0," ++ show n ++ ") {};"

drawDiagram2 :: Diagram2 -> String
drawDiagram2 = undefined


example :: [Cell2]
example = [f,f,f,f]
  where
    a = cell1 "a"
    b = cell1 "b"
    f = cell2 "f" [a] [b]

main :: IO ()
main = do
  putStrLn latexheader
  putStrLn $ drawRack2 example 1
  putStrLn latexclosing

latexheader :: String
latexheader =
  "\\documentclass[tikz, border=2mm]{standalone}\n\\usepackage{tikz}\n\\usetikzlibrary{matrix,arrows,shapes}\n\\tikzset{morphism/.style={circle, minimum height = #1, minimum width = .5cm, fill=white, draw},morphism/.default=.5cm}\n\\begin{document}\n\\begin{tikzpicture}"

latexclosing :: String
latexclosing =
  "\\end{tikzpicture}\n\\end{document}"
