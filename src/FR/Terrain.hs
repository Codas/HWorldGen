module FR.Terrain where

import           Diagrams.Backend.SVG (SVG)
import           Diagrams.Prelude
import           Graphics.SVGFonts

import           FR.Drawing           (Paintable (..), text')
import           FR.Points

data Mountain = Mountain { mPoly :: [NormalP]}

instance Paintable Mountain where
  paint (Mountain points) = undefined
