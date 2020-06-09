-- | This module exemplifies how to use Corda. Some cells are declared and then
-- some diagrams are constructed on top of them.

module Example where

import Chorda

example :: [Cell2]
example = [f,g,h]
  where
    a = cell1 "a"
    b = cell1 "b"
    f = cell2 "f" [a,b] [b]
    d = cell2 "d" [a,b] []
    g = cell2 "g" [a] [b]
    h = cell2 "h" [a] [a,b,b]

example3 :: [[Cell2]]
example3 = [[h],[f,idt a],[d]]
  where
    a = cell1 "a"
    b = cell1 "b"
    f = cell2 "f" [a,b] [b]
    d = cell2 "d" [a,b] []
    g = cell2 "g" [a] [b]
    h = cell2 "h" [a] [a,b,b]


example5 :: [[Cell2]]
example5 = [[ol],[ol,idt a]]
  where
    a = cell1 "\\mathbf{a}"
    ol = cell2 "\\otimes" [a] [a,a]
    t = cell2 "T" [a] [a]
    or = cell2 "\\otimes" [a,a] [a]

example2 :: [Cell1]
example2 = [a,a,a]
  where
    a = cell1 "a"

example6 :: [Diagram2]
example6 =
  [ [[o],[o,idt c]]
  , [[a]]
  , [[o],[idt c, f,idt c]]
  ]
  where
    c = obj "\\mathbb{C}"
    f = morph "f" [] [c]
    o = morph "\\otimes" [c] [c,c]
    a = transf "\\mathrm{id}"

example4 :: [[Cell2]]
example4 = [[ol],[t,idt a],[or]]
  where
    a = cell1 "a"
    ol = cell2 "\\otimes" [a] [a,a]
    t = cell2 "T" [a] [a]
    or = cell2 "\\otimes" [a,a] [a]

exampleAssoc :: [[Cell3]]
exampleAssoc = identify "" [[alpha]]
  where
    c = obj "\\mathbb{C}"
    o = morph "\\otimes" [c] [c,c]
    alpha = cell3 "\\check\\alpha" [[o],[idt c,o]] [[o],[o,idt c]]

    oR = morph "\\otimes" [c,c] [c]
    beta = cell3 "\\hat\\alpha" [[oR,idt c],[oR]] [[idt c,oR],[oR]]


exampleLunit :: [[Cell3]]
exampleLunit = identify "" [[lambda2]]
  where
    c = obj "\\mathbb{C}"
    o = morph "\\otimes" [c] [c,c]
    i = morph "i" [c] []
    lambda2 = cell3 "\\lambda" [[o],[idt c,i],[idt c]] [[idt c]]

exampleTambara :: [[Cell3]]
exampleTambara = identify "" [[y]]
  where
    c = obj "\\mathbb{C}"
    oL = morph "\\otimes" [c] [c,c]
    oR = morph "\\otimes" [c,c] [c]
    t = morph "T" [c] [c]

    y = cell3 "t" [[oR],[t],[oL]] [[idt c, idt c],[t,idt c],[idt c, idt c]]



main :: IO ()
main = do
  -- readFile "latexHeader.tex" >>= putStrLn

  -- EXAMPLE ASSOC
  putStrLn "\\ProvidesPackage{mydiagrams}[2020/05/09 v0.1 My Diagrams.]"
  putStrLn "\\newcommand{\\assocDiagram}{"
  putStrLn "\\begin{tikzpicture}"
  let ex = exampleAssoc
  putStrLn $ drawRack3 1.5 2 ex
  putStrLn $ connectionsRack3 ex
  putStrLn $ drawRack3 1.5 2 ex
  putStrLn "\\end{tikzpicture}"
  putStrLn "}"

  -- EXAMPLE 6
  -- let ex = identify "u" example6
  -- putStrLn $ drawSomeDiagrams 2 ex
  -- putStrLn $ extraConnections ex exampleConn
  -- putStrLn $ drawSomeDiagrams 2 ex
  -- putStrLn $ fili ex

  -- EXAMPLE 4
  -- let ex = identify "u" example4
  -- putStrLn $ drawDiagram2 2 ex

  --  readFile "latexFooter.tex" >>= putStrLn
