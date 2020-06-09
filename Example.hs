-- | This module exemplifies how to use Corda. Some cells are declared and then
-- some diagrams are constructed on top of them.

module Example where

import Chorda



-- We will draw a pseudomonoid (C,⊗,I) in the monoidal bicategory of categories
-- (Cat,×,1), which is the same as a monoidal category. We start by declaring
-- some primitive cells.
c = obj "\\mathbb{C}"
o = morph "\\otimes" [c,c] [c]
i = morph "I" [] [c]
alpha = transf "\\alpha" [[idt c,o],[o]] [[o,idt c],[o]]
lambda = transf "\\lambda" [[i,idt c],[o]] [[idt c],[idt c]]
rho = transf "\\rho" [[idt c,i],[o]] [[idt c],[idt c]]



main :: IO ()
main = do

  -- We now declare the diagrams. We start by labelling our package; then, we
  -- proceed to list the diagrams that should compose our library.
  putStrLn "\\ProvidesPackage{mydiagrams}[2020/05/09 v0.1 My Diagrams.]"

  putStrLn $ unlines $ map show
    [ mkDiagram3D "associatorDiagram" [[alpha]]
    , mkDiagram3D "leftUnitorDiagram" [[lambda]]
    , mkDiagram3D "rightUnitorDiagram" [[rho]]
    ]

  -- readFile "latexHeader.tex" >>= putStrLn

  -- EXAMPLE ASSOC




  -- putStrLn "\\newcommand{\\assocDiagram}{"
  -- putStrLn "\\begin{tikzpicture}" 
  -- let ex = exampleAssoc
  -- putStrLn $ drawRack3 1.5 2 ex
  -- putStrLn $ connectionsRack3 ex
  -- putStrLn $ drawRack3 1.5 2 ex
  -- putStrLn "\\end{tikzpicture}"


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
