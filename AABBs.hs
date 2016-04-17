{-# LANGUAGE NamedFieldPuns #-}
module AABBs where

import Vectors
import Rays


data BoundingBox = BoundingBox Vec3 Vec3

fromPoints :: Vec3 -> Vec3 -> BoundingBox
fromPoints x y = BoundingBox (vzip min x y) (vzip max x y)

includePoint :: BoundingBox -> Vec3 -> BoundingBox
includePoint (BoundingBox bMin bMax) x =
  BoundingBox (vzip min bMin x) (vzip max bMax x)

join :: BoundingBox -> BoundingBox -> BoundingBox
join (BoundingBox min1 max1) (BoundingBox min2 max2) =
  BoundingBox (vzip min min1 min2) (vzip max max1 max2)

wrapCollider :: Collider a -> BoundingBox -> Collider a
wrapCollider cf (BoundingBox bMin bMax) (r@Ray { x0, u }) =
  let slabIntersection (tNear, tFar) dim =
        let ds = getDimension dim
            invRayDir = 1 / (ds u)
            t0 = (ds bMin - ds x0) * invRayDir
            t1 = (ds bMax - ds x0) * invRayDir
            tDimNear = min t0 t1
            tDimFar  = max t0 t1
        in  (max tNear tDimNear, min tFar tDimFar)
      (tNear, tFar) = foldl slabIntersection (0, 1e10) [X, Y, Z]
  in  if tNear > tFar then Nothing else cf r
