module FR.Terrain where

import           Diagrams.Backend.SVG (SVG)
import           Diagrams.Prelude
import           Graphics.SVGFonts

import           FR.Points

data Mountain = Mountain { mPoly :: [NormalP]}
