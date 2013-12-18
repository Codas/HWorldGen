{-# LANGUAGE BangPatterns              #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Main where

import           AStar
import qualified Data.ByteString              as BS
import qualified Data.ByteString.Char8        as C8
import           Data.Matrix                  as M

import           Criterion.Config
import           Criterion.Main
import           Diagrams.Backend.SVG         (renderSVG)
import           Diagrams.Backend.SVG.CmdLine
import           Diagrams.Prelude
-- import           Debug.Trace           as D

main :: IO ()
main = do
    m <- readWorld
    let start = P 21 495 0
        !end  = P 344 395 0
        path  = findPath m start end
        str   = map (\(P x y _) -> [x, y]) $ path
        size  = Absolute
        pathDiag = fromVertices (pathToPoints path) # strokeLine
                   # lc red # lw 2

    putStrLn "looking for path..."
    print path
    renderSVG "path.svg" size $ pathDiag `atop` square 804
    return ()

pathToPoints path = reverse (map (\(P x y _) -> (fromIntegral x) ^& (804 - fromIntegral y)) path)

readWorld :: IO (Matrix Int)
readWorld = do
    bs <- BS.readFile "terrain.txt"
    let rows  = C8.split '\n' bs
        llist = map (C8.split ',') rows
        m     = fromLists (map (map (read . C8.unpack)) llist :: [[Int]])
    return m
