{-# ConstraintKinds #-}
module Kmeans where

import Data.Vector
import Data.List
import qualified Data.Map as M
import Data.Map (Map(..))

type Vvv e v= (Vector v, Vectorizable e v)

clusterAssignment :: (Ord v, Vvv e v)
  => [v] -> [e] -> Map v [e]
clusterAssignment centroids points =
  let initialMap = M.fromList $ zip centroids (repeat [])
  in foldr (\p m ->
             let
               chosenC = minimumBy (compareDistance p) centroids
             in M.adjust (p:) chosenC m)
     initialMap points
  where compareDistance p x y = compare
          (distance x $ toVec p)
          (distance y $ toVec p)

refineCentroids :: (Vvv e v)
  => Map v [e]
  -- ^ a map of vectors to data
  -> [(v, v)]
refineCentroids = M.toList . fmap (centroid . fmap toVector)

shouldStop :: (Vvv e v) => [(v,v)] -> Double -> Bool
shouldStop centroids threshold = foldl (\(x, y) s -> s + distance x y) 0.0 centroids < threshold

kMeans :: (Vvv e v )
  => (Int -> [e] -> [v]) -- ^ init function
  -> Int -- ^ number of centroids
  -> [e] -- ^ data
  -> Double -- ^ threshold
  -> [v] -- ^ final centroids
kMeans init k es = kmeans' (i k points) points

kmeans' :: (Vvv e v)
  => [v] -> [e] -> Double -> [v]
kmeans' centroids points th =
  let
    assignments = clusterAssignment centroids points
    oldNewCentroids = refineCentroids assignments
    newCentroids = snd <$> oldNewCentroids
  in case shouldStop oldNewCentroids th of
    True -> newCentroids
    False -> kmeans' newCentroids points th