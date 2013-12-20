{-# LANGUAGE NoMonomorphismRestriction #-}
module Main where

import qualified Data.ByteString             as BS
import qualified Data.ByteString.Char8       as C8
import           Data.List                   (foldl1)
import           Data.Matrix                 as M

import           Control.Arrow
import           Control.Parallel.Strategies
import           Criterion.Config
import           Criterion.Main              as C
import           Diagrams.Backend.SVG        (SVG, renderSVG)
import           Diagrams.Prelude
-- import           Debug.Trace           as D

import           FR.AStar                    (findPath)
import           FR.Drawing                  (paint)
import           FR.Poi                      (Road (..), Town (..), tLoc)
import           FR.Points                   (FaerunP (..), PLike (np, tp2))
import           FR.Region                   (roads, towns)

main :: IO ()
main = do
    m <- readWorld
    let paths = parMap rseq (\(R s e) -> ptp $ findPath m s e) roads
        size  = Dims 804 804
        pathDiag = position $ map (head &&& road) paths
        pTowns = (position $ map ((tp2 . tLoc) &&& paint) towns)

    putStrLn "looking for path..."
    print $ paths
    renderSVG "path.svg" size $ pTowns `atop` pathDiag `atop` grid
    return ()

ptp :: PLike p => [p] -> [P2]
ptp = map tp2

grid =  mconcat $ map (alignTL . lw 10 . lc gray) [h, v]
  where v = hcat' (with & sep .~ 536 ) (replicate 25 (vrule 12864))
        h = vcat' (with & sep .~ 536 ) (replicate 25 (hrule 12864))

road :: [P2] -> Diagram SVG R2
road path = fromVertices path # strokeLine # lc brown # lw 15

readWorld :: IO (Matrix Int)
readWorld = do
    bs <- BS.readFile "terrain.txt"
    let rows  = C8.split '\n' bs
        llist = map (C8.split ',') rows
        m     = fromLists (map (map (read . C8.unpack)) llist :: [[Int]])
    return m
