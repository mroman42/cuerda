-- | 

module Example2 where

import Chorda

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

example6 :: [Diagram2]
example6 = identify "i"
  [ [[o],[o,idt c]]
  , [[a]]
  , [[o],[idt c, f,idt c]]
  ]
  where
    c = obj "\\mathbb{C}"
    f = morph "f" [] [c]
    o = morph "\\otimes" [c] [c,c]
    a = transf "\\mathrm{id}"


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


example4 :: [[Cell2]]
example4 = [[ol],[t,idt a],[or]]
  where
    a = cell1 "a"
    ol = cell2 "\\otimes" [a] [a,a]
    t = cell2 "T" [a] [a]
    or = cell2 "\\otimes" [a,a] [a]