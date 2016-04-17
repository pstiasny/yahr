module Culling (bvh) where

import Data.List (partition)

import Vectors
import Rays
import AABBs

bvh :: [(BoundingBox, Collider a)] -> Collider a
bvh [] = const Nothing
bvh xs = snd $ buildTree 16 xs
  where
    buildTree :: Int -> [(BoundingBox, Collider a)] -> (BoundingBox, Collider a)
    buildTree _ [] = error "trying to bound nothing"
    buildTree _ [(bb, col)] = (bb, wrapCollider col bb)
    buildTree 0 xs = multiLeaf xs
    buildTree maxDepth xs =
      if   null xs2
      then multiLeaf xs1
      else (jointBB, wrapCollider (collideAll [col1, col2]) jointBB)
      where jointBB = join bb1 bb2
            (bb1, col1) = buildTree (maxDepth - 1) xs1
            (bb2, col2) = buildTree (maxDepth - 1) xs2
            (xs1, xs2) = partition
                           ((<= midpoint) . getDimension dim . centroid . fst)
                           xs
            midpoint = getDimension dim (centroid bbOfCentroids)
            dim = maxExtent bbOfCentroids
            bbOfCentroids = (boundAllPoints (map centroid bbs))
            bbs = map fst xs

    multiLeaf xs = (jointBB, wrapCollider (collideAll colliders) jointBB)
      where jointBB = foldl join (head bbs) (tail bbs)
            (bbs, colliders) = unzip xs
