module Cuerda
  ( module Cell
  , module Identify
  , module Connect
  , module Style
  , module Draw
  , module Print
  , module Position
  , module Cuerda
  , module Settings )
where

import Cell
import Identify
import Connect
import Style
import Draw
import Position
import Print
import Settings


data Diagram3d = Diagram3d
  { commandName3d :: String
  , cells3d :: [[[Cell3]]]
  }

data Diagram2d = Diagram2d
  { commandName2d :: String
  , cells2d :: [[Cell2]]
  }

writeTikz3 :: String -> [[[Cell3]]] -> IO ()
writeTikz3 = writeTikz3withSettings defaultSettings

writeTikz3withSettings :: Settings -> String -> [[[Cell3]]] -> IO ()
writeTikz3withSettings settings filename c = do
  writeFile filename $
    filterEmptyLines $ unlines
      [ "\\documentclass[crop,tikz]{standalone}"
      , "\\usepackage{cuerda}"
      , "\\begin{document}"
      , "\\begin{tikzpicture}[cuerdadiagram]"
      , drawDiagramIn3d settings 2 2.5 (identify "i" c)
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
      , drawDiagram2 defaultSettings 2 (identify "i" c)
      , "\\end{tikzpicture}"
      , "\\end{document}" ]

latex3D :: [[[Cell3]]] -> IO ()
latex3D c = putStr $ filterEmptyLines $ unlines
  [ "\\begin{tikzpicture}[cuerdadiagram]"
  , drawDiagramIn3d defaultSettings 2 2.5 (identify "i" c)
  , "\\end{tikzpicture}" ]

latex2D :: [[Cell2]] -> IO ()
latex2D c = putStr $ filterEmptyLines $ unlines
  [ "\\begin{tikzpicture}[cuerdadiagram]"
  , drawDiagram2 defaultSettings 2 (identify "i" c)
  , "\\end{tikzpicture}" ]

filterEmptyLines :: String -> String
filterEmptyLines = unlines . filter (/= "") . lines

mkDiagram3D :: String -> [[[Cell3]]] -> String
mkDiagram3D s c = show (Diagram3d s (identify "i" c))

instance Show Diagram3d where
  show c = unlines
    [ "\\newcommand{\\" ++ commandName3d c ++ "}{"
    , "\\begin{tikzpicture}[cuerdadiagram]"
    , drawDiagramIn3d defaultSettings 2 2.5 (cells3d c)
    , "\\end{tikzpicture}}" ]

mkDiagram2D :: String -> [[Cell2]] -> String
mkDiagram2D s c = show (Diagram2d s (identify "i" c))

instance Show Diagram2d where
  show c = unlines
    [ "\\newcommand{\\" ++ commandName2d c ++ "}{"
    , "\\begin{tikzpicture}[cuerdadiagram]"
    , drawDiagram2 defaultSettings 2 (cells2d c)
    , "\\end{tikzpicture}}"
    ]


whiteSpider :: [Cell1] -> [Cell1] -> Cell2
whiteSpider = morphWithSettings "circle, minimum height = .25cm, minimum width = .25cm, fill=white, inner sep=0.5pt, draw" ""

blackSpider :: [Cell1] -> [Cell1] -> Cell2
blackSpider = morphWithSettings "circle, minimum height = .25cm, minimum width = .25cm, fill=black, inner sep=0.5pt, draw" ""
