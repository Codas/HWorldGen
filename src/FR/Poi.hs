module FR.Poi where

import           Diagrams.Backend.SVG (SVG)
import           Diagrams.Prelude
import           Graphics.SVGFonts

import           FR.Drawing
import           FR.Points

data Town = T { tLoc  :: NormalP
              , tName :: String
              , tPop  :: Int }
instance PLike Town where
    np (T l _ _) = np l
instance Paintable Town where
    paint t@(T _ n _) = marker === name
      where marker = paint (tType t) # lw 2 # fc black # scale (tSize t)
            name   = text' n 14 blue

tType :: Town -> TownType
tType (T _ _ p)
    | p < 1000  = Village
    | p < 8000  = Town
    | p < 10000 = City
    | otherwise = Metropolis

tSize :: Town -> Double
tSize t@(T _ _ p) = 100 * sqrt (p' / (density * pi) )
  where density = fromIntegral (dMin + (dMax - dMin)) * percentile
        percentile = fromIntegral (p - pMin) / fromIntegral (pMax - pMin)
        (pMin, pMax) = tDens $ tType t
        (dMin, dMax) = tMPop $ tType t
        p' = fromIntegral p

tMPop :: TownType -> (Int, Int)
tMPop Village    = (20,   1000)
tMPop Town       = (1000,  8000)
tMPop City       = (8000,  20000)
tMPop Metropolis = (20000, 200000)
tMPop Capital    = tMPop Metropolis

tDens :: TownType -> (Int, Int)
tDens Village    = (1000,  4000)
tDens Town       = (4000,  6000)
tDens City       = (6000,  10000)
tDens Metropolis = (10000, 250000)
tDens Capital    = tDens Metropolis

data TownType = Metropolis | City | Town | Village | Capital
instance Paintable TownType where
    paint Capital    = square 1
    paint Metropolis = square 1
    paint City       = square 1
    paint Town       = circle 1
    paint Village    = circle 1

data Road = R { from :: NormalP
              , to   :: NormalP }

(<~>) :: Town -> Town -> Road
t1 <~> t2 = R (tLoc t1) (tLoc t2)
