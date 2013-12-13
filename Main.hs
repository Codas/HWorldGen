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
        lt m    = los m (P 1 1 0) (P 400 300 0)

    putStrLn "looking for path..."
    print $ map (\(P x y _) -> [x, y]) $ runAStar dMatrix
    -- print $ lt dMatrix
    -- defaultMain [ bench "AStar" $ nf runAStar dMatrix
    --             , bench "los" $ nf lt dMatrix ]
    return ()

-- heuristic :: DPoint -> Point -> Double
-- heuristic (u, v, _) (x, y, _) = (sqrt((x' - u) ** 2 + (y' - v) ** 2)) * 90
--   where x'    = fromIntegral x
--         y'    = fromIntegral y

los :: Map -> Point -> Point -> (Bool, Double)
los m p@(P a _ _) q@(P b _ _)
    -- | D.trace ("coset: " ++ show cost ++ " points: " ++ show points ++ " l: " ++ show l) False = undefined
    | otherwise = (valid', undefined)
  where !((P xa ya _), (P xb yb _)) = if a < b then (p, q) else (q, p)
        !steep = abs(ya - yb) > abs(xa - xb)
        !(x0, x1) = if steep then (xb, xa) else (xa, xb)
        !(y0, y1) = if steep then (yb, ya) else (ya, yb)
        !sx = x1 - x0
        !sy = y1 - y0
        !dx0 = abs $ 2 * (x0 - x1)
        !dy0 = abs $ 2 * (y0 - y1)
        !lowest = fromIntegral $ min (m ! (x0, y0)) (m ! (x1, y1))
        (valid', !cost, !points) = los' (x0 + 1) y0 dx0 dy0 lowest (True, 0, 0)
        los' :: Int -> Int -> Int -> Int -> Double -> Double -> (Bool, Int, Int) -> (Bool, Int, Int)
        los' !x !y !dx !dy w low (clear, !c, !n)
            -- | D.trace ("finished: " ++ show (x >= x1) ++ ", canceled: " ++ show (high - 1 > low)  ++ ", low: " ++ show low ++ ", current: " ++ show (m ! (x, y)) ) False = undefined
            | x >= x1   = (clear, nc, n+1)
            | high - 15 > low = recurse False
            | otherwise = recurse clear
                where !nd = (+) dx (if dx > 0 then 2*(dy-dx) else 2*dy)
                      !ny =         if dx > 0 then y+1       else y
                      !c' = m ! (x, y)
                      !nc = c + c'
                      !low' = min low $ fromIntegral c'
                      !high = fromIntegral c'
                      recurse cl = los' nx ny nx ndy low' (clear, nc, n+1)

successor :: Map -> Point -> [Point]
successor m = successor'
  where inM (P x y _) = y <= r && x <= c && y > 0 && x > 0 && (m ! (x, y) /= 255)
        !r = nrows m
        !c = ncols m
        successor' (P x y _) = filter inM candidates
          where candidates = [(P (x-1) (y-1) 0), (P x (y-1) 0), (P (x+1) (y-1) 0)
                             ,(P (x-1) y   0),                  (P (x+1) y   0)
                             ,(P (x-1) (y+1) 0), (P x (y+1) 0), (P (x+1) (y+1) 0)]

cost :: Map -> Point -> Point -> Point -> [Point]
cost m a@(P x0 y0 ca) b@(P x y cb) c@(P u v _)
  | D.trace ("coord: " ++ show [a,b,c] ++ " smaller: " ++ show (ca > cb) ++ " d: " ++ show (round d') ++ " dir: " ++ show (round $ directCost + cb) ++ " los: " ++ show (round $ losD + ca)) False = path
  | otherwise = path
  where (hasLos, losD) = los m a c
        directCost = rawCost * d
        rawCost    = abs $ fromIntegral $ m ! (u, v)
        path       = if hasLos then [c',a] else [c',b,a]
        c' = (P u v nc)
        !nc = if hasLos then directCost + cb else losD + ca
        d  = abs $ sqrt $ (dx ** 2 + dy ** 2)
        d' = abs $ sqrt $ (dx' ** 2 + dy' ** 2)
        dx = fromIntegral $ x - u
        dy = fromIntegral $ y - v
        dx' = fromIntegral $ x0 - u
        dy' = fromIntegral $ y0 - v

runAStar :: Map -> [Point]
runAStar m = astar start (== end) succ (cost m) h m
 where start = (P 21 495 (fromIntegral $ m ! (21, 495)))
       end   = (P 344 395 0)
       dEnd  = (DP 344 395 0)
       succ  = successor m
       h     = (* 0.8) . snd . los m end
