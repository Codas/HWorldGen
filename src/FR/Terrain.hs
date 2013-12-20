{-# LANGUAGE NoMonomorphismRestriction #-}
module FR.Terrain where

import           Diagrams.Backend.SVG (SVG)
import           Diagrams.Prelude
import           Diagrams.TwoD.Offset
import           Graphics.SVGFonts

import           FR.Drawing           (Paintable (..), text')
import           FR.Points

data TType = Mountain | Forest | Sea | Swamp | Lake

data Terrain = Terrain { tType :: TType
                       , mPoly :: [NormalP]}

instance Paintable Terrain where
    paint (Terrain t ps) = trail # adjustTerrain t
                                where trail = fromVertices (map tp2 ps) # closePath

instance PLike Terrain where
    np (Terrain _ ps) = head ps

adjustTerrain :: TType -> Path R2 -> Diagram SVG R2
adjustTerrain tt path = mconcat $ diag' : map toDiag paths
  where paths = offsetTerrain n d path
        diag' = (path # stroke # fc c # lw 15 # lc (blend 0.5 black c) # opacity o)
        toDiag = fc c . lw 0 . opacity (1 / (n + 1)) . stroke
        (c, o, n, d) = case tt of
                            Mountain -> (saddlebrown,  0.8, 3, 96)
                            Forest   -> (forestgreen,  0.8, 2, 96)
                            Sea      -> (darkblue,     1.0, 0, 0)
                            Lake     -> (royalblue,    0.8, 0, 0)
                            Swamp    -> (darkkhaki,    0.8, 1, 64)



offsetTerrain :: Int -> Int -> Path R2 -> [Path R2]
offsetTerrain n d p = map (\i -> offsetPath (fromIntegral $ step * i) p) [1..n]
  where step = d `div` n


closePath = pathFromTrail . wrapLoop . reverseLoop . closeLine
