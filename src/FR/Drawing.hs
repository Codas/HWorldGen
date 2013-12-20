module FR.Drawing where

import           Diagrams.Backend.SVG (SVG)
import           Diagrams.Prelude
import           Graphics.SVGFonts


class Paintable a where
    paint :: a -> Diagram SVG R2

text' t h c = stroke (textSVG' $ TextOpts t lin INSIDE_H KERN False s s )
              # fc c # lc c # fillRule EvenOdd
  where s = h * 20
