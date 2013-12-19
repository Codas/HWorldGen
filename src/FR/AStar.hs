{-# LANGUAGE BangPatterns #-}
module FR.AStar (findPath) where

import           Data.IntSet          (IntSet)
import qualified Data.IntSet          as S
import           Data.Matrix          (Matrix, ncols, nrows, (!))
import qualified Data.PQueue.Prio.Min as Q
-- import           Debug.Trace          as D
import           Prelude

import           FR.Points

data Point = P
             {-# UNPACK #-} !Int
             {-# UNPACK #-} !Int
             {-# UNPACK #-} !Double
             deriving (Ord)
instance Eq Point where
  (P a1 b1 _) == (P a2 b2 _) = a1 == a2 && b1 == b2
instance Show Point where
  show (P x y c) = "(" ++ show x ++ "," ++ show y++"):" ++ show (round c :: Int)

type Map = Matrix Int

getCost :: Point -> Double
getCost (P _ _ t) = t

astar :: Point               -- ^ start point
      -> (Point -> Bool)     -- ^ Point is End?
      -> (Point -> [Point])  -- ^ succ function
      -> (Point -> Point -> Point -> [Point])
      -> (Point -> Double)   -- ^ heuristic function
      -> Matrix m            -- ^ the playground
      -> [Point]
astar start end succ cost heur m
    = astar' (S.singleton $ toInt start) (Q.singleton (heur start) [start,start])
  where r = nrows m
        toInt (P x y _) = x * r + y
        astar' !seen !q
            -- | D.trace (show (end l) ++ " - " ++ (show $ Q.findMin q) ++ "\n") False = undefined
            | Q.null q  = error "No Solution."
            | end l     = path
            | otherwise = astar' seen' q'
          where ((c, path@(l:pre:rest)), dq) = Q.deleteFindMin q
                !succs = filter ((`S.notMember` seen) . toInt) $ succ l
                !costs = map (cost pre l ) succs
                pq     = map (\np@(n:_) -> ((getCost n) + (heur n), np ++ rest)) costs
                !q'    = dq `Q.union` Q.fromList pq
                !seen' = seen `S.union` S.fromList (map toInt succs)


heuristic :: Point -> Point -> Double
heuristic (P u v _) (P x y _) = (sqrt((x' - u') ** 2 + (y' - v') ** 2)) * 95
  where x'    = fromIntegral x
        y'    = fromIntegral y
        u'    = fromIntegral u
        v'    = fromIntegral v

los :: Map -> Point -> Point -> (Bool, Double)
los m (P startX startY _) (P endX endY _) = (valid', fc)
  where (!vxi, !vyi) = (endX - startX, endY - startY)
        !switch = abs (endX - startX) < abs (endY - startY)
        tileCost (!tx, !ty) = m ! if switch then (ty, tx) else (tx, ty)
        (!vx, !vy) = if switch then (vyi, vxi) else (vxi, vyi)
        (!x0, !y0) = if switch then (startY, startX) else (startX, startY)
        (!x1, !y1) = if switch then (endY, endX) else (endX, endY)
        !sx  = signum vx   -- signum funktion, negativ: -1, 0: 0, positiv: +1
        !sy  = signum vy   -- Schritte in x, y richtung
        !dx = abs $ 2 * vx -- absolutes delta, wieso *2?
        !dy = abs $ 2 * vy
        !c0  = - tileCost (x0, y0)  -- Initial cost
        !e0  = 0  -- initial error
        !w0  = 0
        !p   = (dy + dx) `div` 2
        !t   = (dy - dx) `div` 2
        !fc  = cost * ca
        !ca = if dx /= 0 then sqrt (fromIntegral dx ** 2 + fromIntegral dy ** 2) / fromIntegral dx else cost
        !lowest = fromIntegral $ min (m ! (x0, y0)) (m ! (x1, y1)) -- lowest current cost.
        (valid', !cost) = los' x0 y0 c0 e0 w0 lowest True  -- begin iteration
        los' :: Int -> Int -> Int -> Int -> Int -> Int -> Bool -> (Bool, Double)
        los' !x !y !c !e !w !low clear
            | x == x1   = (clear, fromIntegral nc)            -- When x == x1, return cleat status and costs
            | high - 15 > low = recurse False    -- when worse terrain encountered, set LOS to false, but continue
            | otherwise = recurse clear          -- we're not done yet, continue
                where (!ne, !nc, !nw, !nx, !ny)
                          | e > t && (p + e < dy) = ((e - dy), (c + w * c'), ((p + e) `div` dy), (x + sx), y)
                          | e > t = ((e - dy), (c + c'), w, (x + sx), y)
                          | e < t = ((e + dx), (c + (1-w) * c'), w, x, (y + sy))
                          | otherwise = ((e + dx - dy), (c + c'), w, (x + sx), (y + sy))
                      !c' = tileCost (x, y) -- current field costs
                      !low' = min low c'                  -- new cost minimum
                      !high = c'  -- naming alias to make things more explicit
                      recurse = los' nx ny nc ne nw low'  -- continue with LOS status

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
  -- | D.trace ("coord: " ++ show [a,b,c] ++ " smaller: " ++ show (ca > cb) ++ " dir: " ++ show (round $ directCost + cb) ++ " los: " ++ show (round $ losD + ca)) False = path
  | otherwise = path
  where (hasLos, losD) = los m a c
        directCost = rawCost * d + swCost
        rawCost    = abs $ fromIntegral $ m ! (u, v)
        path       = if hasLos then [c',a] else [c',b,a]
        swCost     = fromIntegral $ (abs $ (m ! (u, v)) - (m ! (x, y))) * 4
        c' = (P u v nc)
        !nc = if hasLos && losD + ca < directCost + cb then losD + ca else directCost + cb
        d  = abs $ sqrt $ (dx ** 2 + dy ** 2)
        d' = abs $ sqrt $ (dx' ** 2 + dy' ** 2)
        dx = fromIntegral $ x - u
        dy = fromIntegral $ y - v
        dx' = fromIntegral $ x0 - u
        dy' = fromIntegral $ y0 - v

findPath :: PLike p => Map -> p -> p -> [MapP]
findPath m s e = tdp $ astar start (== end) succ (cost m) h m
  where succ  = successor m
        start = P sx sy 0
        end   = P ex ey 0
        h     = heuristic end
        (MP sx sy) = mp s
        (MP ex ey) = mp e
        tdp = map (\(P x y _)-> (MP x y))
