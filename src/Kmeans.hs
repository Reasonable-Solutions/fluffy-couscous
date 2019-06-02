module Kmeans where

import Data.Vector

kMeans :: (Vector v, Vectorizable e v)
  => Int
  -- ^ number of centroids
  -> [e]
  -- ^ the data
  -> [v]
  -- ^ centroids after convergence
kMeans = undefined