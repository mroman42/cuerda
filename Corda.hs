module Cuerda
  ( module Cell
  , module Identify
  , module Connect
  , module Style
  , module Draw
  , module Print
  , module Position
  , module Cuerda )
where

import Cell
import Identify
import Connect
import Style
import Draw
import Position
import Print



data Diagram3d = Diagram3d
  { commandName3d :: String
  , cells3d :: [[[Cell3]]]
  }

data Diagram2d = Diagram2d
  { commandName2d :: String
  , cells2d :: [[Cell2]]
  }

writeTikz3 :: String -> [[[Cell3]]] -> IO ()
writeTikz3 filename c = do
  writeFile filename $
    filterEmptyLines $ unlines
      [ "\\documentclass[crop,tikz]{standalone}"
      , "\\usepackage{cuerda}"
      , "\\begin{document}"
      , "\\begin{tikzpicture}[cuerdadiagram]"
      , drawDiagramIn3d 2 2.5 (identify "i" c)
      , "\\end{tikzpicture}"
      , "\\end{document}" ]

writeTikz2 :: String -> Diagram2 -> IO ()
writeTikz2 filename c = do
  writeFile filename $
    filterEmptyLines $ unlines
      [ "\\documentclass[crop,tikz]{standalone}"
      , "\\usepackage{cuerda}"
      , "\\begin{document}"
      , "\\begin{tikzpicture}[cuerdadiagram]"
      , drawDiagram2 2 (identify "i" c)
      , "\\end{tikzpicture}"
      , "\\end{document}" ]

latex3D :: [[[Cell3]]] -> IO ()
latex3D c = putStr $ filterEmptyLines $ unlines
  [ "\\begin{tikzpicture}[cuerdadiagram]"
  , drawDiagramIn3d 2 2.5 (identify "i" c)
  , "\\end{tikzpicture}" ]

latex2D :: [[Cell2]] -> IO ()
latex2D c = putStr $ filterEmptyLines $ unlines
  [ "\\begin{tikzpicture}[cuerdadiagram]"
  , drawDiagram2 2 (identify "i" c)
  , "\\end{tikzpicture}" ]

filterEmptyLines :: String -> String
filterEmptyLines = unlines . filter (/= "") . lines

mkDiagram3D :: String -> [[[Cell3]]] -> String
mkDiagram3D s c = show (Diagram3d s (identify "i" c))

instance Show Diagram3d where
  show c = unlines
    [ "\\newcommand{\\" ++ commandName3d c ++ "}{"
    , "\\begin{tikzpicture}[cuerdadiagram]"
    , drawDiagramIn3d 2 2.5 (cells3d c)
    , "\\end{tikzpicture}}" ]

mkDiagram2D :: String -> [[Cell2]] -> String
mkDiagram2D s c = show (Diagram2d s (identify "i" c))

instance Show Diagram2d where
  show c = unlines
    [ "\\newcommand{\\" ++ commandName2d c ++ "}{"
    , "\\begin{tikzpicture}[cuerdadiagram]"
    , drawDiagram2 2 (cells2d c)
    , "\\end{tikzpicture}}"
    ]



-------------
-- EXAMPLE --
-------------
-- c = obj "\\mathbb{C}"
-- o = morph "\\otimes" [c,c] [c]
-- i = morph "I" [] [c]
-- alpha = transf "\\hat\\alpha" [[idt c,o],[o]] [[o,idt c],[o]]
-- alphainv = transf "\\hat\\alpha" [[o,idt c],[o]] [[idt c,o],[o]]

-- main :: IO ()
-- main = do
--   writeTikz3 "test1.tex" [[[alphainv]],[[alpha]]]
--   writeTikz2 "test2.tex" [[idt c,idt c,i],[idt c,o],[o]]
