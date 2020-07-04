-- | Settings for drawing the diagram

module Settings where

data Settings = Settings
  { morphismStyle :: String
  , wireStyle :: String
  , borderLeftStyle :: String
  , borderRightStyle :: String
  , transformationStyle :: String
  }

defaultSettings :: Settings
defaultSettings = Settings
    defaultMorphismStyle
    defaultWireStyle
    defaultBordersStyle
    defaultBordersStyle
    defaultTransformationStyle

defaultObjectStyle :: String
defaultObjectStyle =
  "color={rgb,255:red,46; green,52; blue,64}"

defaultMorphismStyle :: String
defaultMorphismStyle =
  "circle, minimum height = .4cm, minimum width = .4cm, fill=white, inner sep=1pt, draw={rgb,255:red,46; green,52; blue,64}, text={rgb,255:red,46; green,52; blue,64}"

defaultWireStyle :: String
defaultWireStyle =
  "draw={rgb,255:red,191; green,97; blue,106}, opacity=0.5"

defaultBordersStyle :: String
defaultBordersStyle =
  "draw={rgb,255:red,191; green,97; blue,106}, opacity=0.5, dashed"

defaultTransformationStyle :: String
defaultTransformationStyle =
  "rectangle, rounded corners, minimum height = .4cm, minimum width = .4cm, fill=white, inner sep=1pt, draw={rgb,255:red,191; green,97; blue,106}, text={rgb,255:red,191; green,97; blue,106}"
