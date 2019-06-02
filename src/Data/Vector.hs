{-# Language FlexibleInstances     #-}
{-# Language FlexibleContexts      #-}
{-# Language InstanceSigs          #-}
{-# Language KindSignatures        #-}
{-# Language MultiParamTypeClasses #-}
module Data.Vector where

type V2 = (Double, Double)
class (Ord v) => Vector (v :: *) where
  distance :: v -> v -> Double
  centroid :: [v] -> v

class Vector v => Vectorizable e v where
  toVec :: e -> v

instance Vector V2 where
  distance :: V2 -> V2 -> Double
  distance (x1, y1) (x2, y2) = sqrt $ ((x2-x1)**2.0) + ((y2 - y1)**2.0)

  centroid :: [V2] -> V2
  centroid vs = let
    (sumx, sumy) = foldl (\(x,y) (x0,y0)-> (x+x0, y+y0)) (0.0,0.0) vs
    len = fromIntegral . length $ vs
    in (sumx / len, sumy / len)

 -- >>> distance (0.0,0.0) (1.0,1.0)
-- 1.4142135623730951

 -- >>> centroid [(110.0,0.0):: V2, (1.0,1.0), (0.0,1.0), (1.0,0.0)]
-- (28.0,0.5)
