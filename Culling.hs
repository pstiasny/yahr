module Culling (
  cull,
  CullingMode (BVH),
  SplitMode (Midpoint, SurfaceAreaHeuristic)
  ) where

import Control.Exception (assert)
import Data.Maybe
import Data.List (partition, minimumBy)
import Data.Function (on)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV

import Vectors
import Rays
import AABBs

data SplitMode = Midpoint | SurfaceAreaHeuristic deriving (Show, Read)
data CullingMode = BVH Int SplitMode deriving (Show, Read)

cull :: CullingMode -> [(BoundingBox, Collider a)] -> Collider a
cull (BVH maxDepth splitMode) = bvh maxDepth splitMode

collideBVHNodes (bb1, col1) (bb2, col2) r =
  collideAll [col1, col2] r

bvh :: Int -> SplitMode -> [(BoundingBox, Collider a)] -> Collider a
bvh maxDepth splitMode [] = const Nothing
bvh maxDepth splitMode xs = snd $ buildTree maxDepth xs
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

            (xs1, xs2) =
              case splitMode of
                SurfaceAreaHeuristic -> sahSplit xs dim bbOfCentroids
                Midpoint -> midpointSplit xs dim bbOfCentroids 

            dim = maxExtent bbOfCentroids
            bbOfCentroids@(BoundingBox min max) = boundAllPoints (map centroid bbs)
            bbs = map fst xs

    multiLeaf xs = (jointBB, wrapCollider (collideAll colliders) jointBB)
      where jointBB = foldl join empty bbs
            (bbs, colliders) = unzip xs

midpointSplit :: [(BoundingBox, a)] -> Dimension -> BoundingBox
                   -> ([(BoundingBox, a)], [(BoundingBox, a)])
midpointSplit xs dim bbOfCentroids =
  partition ((<= midpoint) . getDimension dim . centroid . fst) xs
  where midpoint = getDimension dim (centroid bbOfCentroids)
              
sahSplit :: [(BoundingBox, a)] -> Dimension -> BoundingBox
              -> ([(BoundingBox, a)], [(BoundingBox, a)])
sahSplit xs dim bbOfCentroids@(BoundingBox bMin bMax) =
  let nBuckets = 16 :: Int

      centMin = getDimension dim bMin
      centMax = getDimension dim bMax

      bucketId :: BoundingBox -> Int
      bucketId bb =
        min (nBuckets - 1) $
          max 0 $
          floor $
          (fromIntegral nBuckets) * frac
              
        where frac = d / len
              len = centMax - centMin
              d = getDimension dim (centroid bb) - centMin


      buckets :: V.Vector (Int, BoundingBox)
      buckets = V.create $ do
        v <- MV.replicate nBuckets (0, empty)
        let addBox bb = do
              let i = bucketId bb
              (c, bbs) <- MV.read v i
              MV.write v i (c + 1, join bbs bb)
        mapM_ addBox (map fst xs)
        return v

      wrappedArea :: Float
      wrappedArea = foldl (\x (_, bb) -> x + surf bb) 0 buckets

      costOfSplitAt :: Int -> Float
      costOfSplitAt i =
          0.125 + (count0 * surf b0 + count1 * surf b1) / wrappedArea
        where b0 = V.foldl join empty $ V.map snd buckets0
              b1 = V.foldl join empty $ V.map snd buckets1
              count0 = fromIntegral $ V.sum $ V.map fst buckets0
              count1 = fromIntegral $ V.sum $ V.map fst buckets1
              (buckets0, buckets1) = V.splitAt (i+1) buckets

      splitIds =[0..(nBuckets - 2)]
      costs = map costOfSplitAt splitIds

      minSplit :: (Int, Float)
      minSplit = minimumBy (compare `on` snd) $ zip splitIds $ costs

  in  if length xs > 4
           then partition ((<= (fst minSplit)) . bucketId . fst) xs
           else midpointSplit xs dim bbOfCentroids
