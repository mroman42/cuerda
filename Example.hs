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
alphainv = transf "\\alpha" [[o,idt c],[o]] [[idt c,o],[o]]
lambda = transf "\\lambda" [[i,idt c],[o]] [[idt c],[idt c]]
lambdainv = transf "\\lambda" [[idt c],[idt c]] [[i,idt c],[o]]
rho = transf "\\rho" [[idt c,i],[o]] [[idt c],[idt c]]
rhoinv = transf "\\rho" [[idt c],[idt c]] [[idt c,i],[o]]


main :: IO ()
main = do

  -- We now declare the diagrams. We start by labelling our package; then, we
  -- proceed to list the diagrams that should compose our library.
  putStrLn "\\ProvidesPackage{mydiagrams}[2020/05/09 v0.1 My Diagrams.]"

  putStrLn $ unlines $ map show
    [ mkDiagram3D "associatorDiagram" [[[alphainv]],[[alpha]]]
    , mkDiagram3D "leftUnitorDiagram" [[[lambdainv]],[[lambda]]]
    , mkDiagram3D "rightUnitorDiagram" [[[rhoinv]],[[rho]]]
    ]
