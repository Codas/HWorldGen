{-# LANGUAGE BangPatterns #-}
module Main where

import           AStar
import           Control.DeepSeq
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as C8
import           Data.Matrix           as M

import           Criterion.Main
import           Debug.Trace           as D

main :: IO ()
main = do
    bs <- BS.readFile "terrain.txt"
    let rows    = C8.split '\n' bs
        llist   = map (C8.split ',') rows
        dMatrix = fromLists (map (map (read . C8.unpack)) llist :: [[Int]])
        lt m    = los m (1, 1, 0) (400, 300, 0)

    putStrLn "looking for path..."
    print $ map (\(x, y, _) -> [x, y]) $ runAStar dMatrix
    -- print $ lt dMatrix
    -- defaultMain [ bench "AStar" $ nf runAStar dMatrix
    --             , bench "los" $ nf lt dMatrix ]
    return ()

heuristic :: DPoint -> Point -> Double
heuristic (u, v, _) (x, y, _) = (sqrt((x' - u) ** 2 + (y' - v) ** 2)) * 115
  where x'    = fromIntegral x
        y'    = fromIntegral y

los :: Map -> Point -> Point -> (Bool, Double)
los m p@(a, _, _) q@(b, _, _) = (valid', fc)
  where !((xa, ya, _), (xb, yb, _)) = if a < b then (p, q) else (q, p)
        !steep = abs(ya - yb) > abs(xa - xb)
        !(x0, x1) = if steep then (xb, xa) else (xa, xb)
        !(y0, y1) = if steep then (yb, ya) else (ya, yb)
        !dx = x1 - x0
        !dy = y1 - y0
        !ddx = fromIntegral dx
        !ddy = fromIntegral dy
        !d0 = 2 * dy - dx
        !fc = fromIntegral cost / fromIntegral points * l
        !l = sqrt (ddx * ddx + ddy * ddy)
        !c0 = m ! (x0, y0)
        (valid', !cost, !points) = los' (x0 + 1) y0 d0 (True, c0, 1)
        los' :: Int -> Int -> Int -> (Bool, Int, Int) -> (Bool, Int, Int)
        los' !x !y !d (_, !c, !n)
            | m ! (x, y) - 1 > c0 = (False, 0, n)
            | x >= x1   = (True, nc, n+1)
            | otherwise = los' nx ny nd (True, nc, n+1)
                where !nx = x + 1
                      !nd = (+) d (if d > 0 then 2*dy - 2*dx else 2*dy)
                      !ny =        if d > 0 then y+1         else y
                      !nc = c + fromIntegral (m ! (x, y))

successor :: Map -> Point -> [Point]
successor m = successor'
  where inM (x, y, _) = y <= r && x <= c && y > 0 && x > 0 && (m ! (x, y) /= 255)
        !r = nrows m
        !c = ncols m
        successor' (x, y, _) = filter inM candidates
          where candidates = [(x-1, y-1, 0), (x, y-1, 0), (x+1, y-1, 0)
                             ,(x-1, y  , 0),              (x+1, y,   0)
                             ,(x-1, y+1, 0), (x, y+1, 0), (x+1, y+1, 0)]

cost :: Map -> Point -> Point -> Point -> [Point]
cost m a@(_, _, ca) b@(x, y, cb) c@(u, v, _) = path
  where (hasLos, losD) = los m a c
        directCost = rawCost * d
        rawCost    = fromIntegral $ m ! (u, v)
        path       = if hasLos then [c',a] else [c',b,a]
        c' = (u, v, nc)
        !nc = if hasLos then directCost + cb else losD + ca
        d  = sqrt $ fromIntegral (dx + dy)
        dx = abs $ x - u
        dy = abs $ y - v

runAStar :: Map -> [Point]
runAStar m = astar start (isEnd end) succ (cost m) h m
 where start = (21, 495, 0)
       end   = (344, 395, 0)
       isEnd (x, y, _) (u, v, _) = x == u && y == v
       dEnd  = (344, 395, 0)
       succ  = successor m
       h     = heuristic dEnd
