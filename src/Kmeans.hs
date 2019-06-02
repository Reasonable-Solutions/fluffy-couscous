module Kmeans where

import Data.Vector
import Data.List
import qualified Data.Map as M
import Data.Map (Map(..))

kMeans :: (Vector v, Vectorizable e v)
  => (Int -> [e] -> [v])
  -- ^ number of centroids
  -> [e]
  -- ^ the data
  -> [v]
  -- ^ centroids after convergence
kMeans = undefined

clusterAssignment :: (Ord v, Vector v, Vectorizable e v)
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