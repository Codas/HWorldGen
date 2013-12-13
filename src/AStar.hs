{-# LANGUAGE BangPatterns #-}
module AStar where

import           Control.DeepSeq
import           Control.Monad           (liftM2)
import           Control.Monad.Instances
import           Data.IntSet             (IntSet)
import qualified Data.IntSet             as S
import           Data.List               (foldl')
import           Data.Matrix             (Matrix, nrows)
import qualified Data.PQueue.Prio.Min    as Q
import           Debug.Trace             as D
-- import           Prelude

type Point   = (Int, Int, Double)
type DPoint  = (Double, Double, Double)

type Map = Matrix Int

trd (_, _, t) = t

astar :: Point               -- ^ start point
      -> (Point -> Bool)     -- ^ Point is End?
      -> (Point -> [Point])  -- ^ succ function
      -> (Point -> Point -> Point -> [Point])
      -> (Point -> Double)
      -> Matrix m
      -> [Point]
astar start end succ cost heur m
    = astar' (S.singleton $ toInt start) (Q.singleton (heur start) [start,start])
  where r = nrows m
        toInt (x, y, _) = x * r + y
        astar' !seen !q
            | D.trace ((show $ end l) ++ " - " ++ (show $ Q.findMin q) ++ "\n") False = undefined
            | Q.null q  = error "No Solution."
            | end l     = path
            | otherwise = astar' seen' q'
          where ((c, path@(l:pre:rest)), dq) = Q.deleteFindMin q
                !succs = filter ((`S.notMember` seen) . toInt) $ succ l
                !costs = map (cost pre l ) succs
                pq     = map (\np@(n:_) -> (c - (heur l) + (heur n), np ++ rest)) costs
                !q'    = dq `Q.union` Q.fromList pq
                !seen' = seen `S.union` S.fromList (map toInt succs)
