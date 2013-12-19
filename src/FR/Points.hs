{-# LANGUAGE BangPatterns #-}
module FR.Points where

import           Control.DeepSeq
import           Diagrams.Backend.SVG (SVG)
import           Diagrams.Prelude


-- Ferun Points
data FaerunP = FP {-# UNPACK #-} !Int
                  {-# UNPACK #-} !Int
                  deriving (Ord, Eq, Show)
instance PLike FaerunP where
  np (FP x y) = (DP (26 * x) (26 * y))

-- Direct Points
data NormalP = DP {-# UNPACK #-} !Int
                  {-# UNPACK #-} !Int
                  deriving (Ord, Eq, Show)
instance PLike NormalP where
  np = id

-- Map points
data MapP = MP {-# UNPACK #-} !Int
               {-# UNPACK #-} !Int
               deriving (Ord, Eq, Show)

instance PLike MapP where
  np (MP x y) = (DP (16 * x) (16 * y))

-- strictnesss stuff
instance NFData MapP where
    rnf (MP x y) = rnf x `seq` rnf y
instance NFData NormalP where
    rnf (DP x y) = rnf x `seq` rnf y
instance NFData FaerunP where
    rnf (FP x y) = rnf x `seq` rnf y

class PLike a where
  np :: a -> NormalP
  fp :: a -> FaerunP
  fp !p = toF $ np p
    where toF (DP x y) = FP (x `div` 26) (y `div` 26)
  mp :: a -> MapP
  mp !p = toM $ np p
    where toM (DP x y) = MP (x `div` 16) (y `div` 16)
  tp2 :: a -> P2
  tp2 p = toP2 $ np p
    where toP2 (DP x y) = fromIntegral x ^& (- fromIntegral y)
