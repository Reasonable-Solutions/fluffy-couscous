{-# Language FlexibleInstances     #-}
{-# Language InstanceSigs          #-}
{-# Language KindSignatures        #-}
{-# Language MultiParamTypeClasses #-}
module Data.Vector where

class Vector (v :: *) where
  distance :: v -> v -> Double

class Vector v => Vectorizable e v where
  toVec :: e -> v
  
instance Vector (Double, Double) where
  distance :: (Double, Double) -> (Double, Double) -> Double
  distance (x1, y1) (x2, y2) = sqrt $ (x2-x1)**2.0 * (y2 - x2)**2.0

