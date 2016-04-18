{-# LANGUAGE NamedFieldPuns #-}
module AABBs where

import Vectors
import Rays


data BoundingBox = BoundingBox Vec3 Vec3 deriving Show

empty = BoundingBox (vof inf) (vof (-inf))
  where inf = read "Infinity"

fromPoints :: Vec3 -> Vec3 -> BoundingBox
fromPoints x y = BoundingBox (vzip min x y) (vzip max x y)

includePoint :: BoundingBox -> Vec3 -> BoundingBox
includePoint (BoundingBox bMin bMax) x =
  BoundingBox (vzip min bMin x) (vzip max bMax x)

boundAllPoints :: [Vec3] -> BoundingBox
boundAllPoints [] = error "No bound for empty point set"
boundAllPoints [x] = fromPoints x x
boundAllPoints (x1:x2:t) = foldl includePoint (fromPoints x1 x2) t

join :: BoundingBox -> BoundingBox -> BoundingBox
join (BoundingBox min1 max1) (BoundingBox min2 max2) =
  BoundingBox (vzip min min1 min2) (vzip max max1 max2)

bbRayIntersection :: BoundingBox -> Ray -> Maybe Float
bbRayIntersection (BoundingBox bMin bMax) (r@Ray { x0, u, tMax }) =
  let slabIntersection (tNear, tFar) dim =
        let ds = getDimension dim
            invRayDir = 1 / (ds u)
            t0 = (ds bMin - ds x0) * invRayDir
            t1 = (ds bMax - ds x0) * invRayDir
            tDimNear = min t0 t1
            tDimFar  = max t0 t1
        in  (max tNear tDimNear, min tFar tDimFar)
      (tNear, tFar) = foldl slabIntersection (0, tMax) [X, Y, Z]
  in  if tNear > tFar then Nothing else Just tNear

wrapCollider :: Collider a -> BoundingBox -> Collider a
wrapCollider cf bb r = bbRayIntersection bb r >>= \t -> cf r

maxExtent :: BoundingBox -> Dimension
maxExtent (BoundingBox pMin pMax) = maxDimension $ pMax - pMin

centroid :: BoundingBox -> Vec3
centroid (BoundingBox pMin pMax) = 0.5 @* pMin + 0.5 @* pMax

surf :: BoundingBox -> Float
surf (BoundingBox bMin bMax) = 2 * (dx * dy + dx * dz + dy * dz)
  where Vec3 dx dy dz = bMax - bMin
