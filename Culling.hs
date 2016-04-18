module Culling (bvh) where

import Data.Maybe
import Data.List (partition)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV

import Vectors
import Rays
import AABBs

collideBVHNodes (bb1, col1) (bb2, col2) r =
  collideAll [col1, col2] r

bvh :: [(BoundingBox, Collider a)] -> Collider a
bvh [] = const Nothing
bvh xs = snd $ buildTree 16 xs
  where
    buildTree :: Int -> [(BoundingBox, Collider a)] -> (BoundingBox, Collider a)
    buildTree _ [] = (empty, nullCollider)
    buildTree _ [(bb, col)] = (bb, wrapCollider col bb)
    buildTree 0 xs = multiLeaf xs
    buildTree maxDepth xs =
      if   null xs2
      then multiLeaf xs1
      else (jointBB, wrapCollider (collideBVHNodes x1 x2) jointBB)
      where jointBB = join bb1 bb2
            x1@(bb1, col1) = buildTree (maxDepth - 1) xs1
            x2@(bb2, col2) = buildTree (maxDepth - 1) xs2
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
