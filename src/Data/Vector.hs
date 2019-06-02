{-# Language FlexibleInstances     #-}
{-# Language FlexibleContexts      #-}
{-# Language InstanceSigs          #-}
{-# Language KindSignatures        #-}
{-# Language MultiParamTypeClasses #-}
module Data.Vector where

class (Ord v) => Vector (v :: *) where
  distance :: v -> v -> Double
  centroid :: [v] -> v

class Vector v => Vectorizable e v where
  toVec :: e -> v

instance Vector (Double, Double) where
  distance (x1, y1) (x2, y2) = sqrt $ ((x2-x1)**2.0) + ((y2 - y1)**2.0)
  centroid vs = let
    (sumx, sumy) = foldl (\(x,y) (x0,y0)-> ((x+x0), (y+y0))) (0.0,0.0) vs
    len = fromIntegral . length $ vs
    in (sumx / len, sumy / len)