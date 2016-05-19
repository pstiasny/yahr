{-# LANGUAGE NamedFieldPuns #-}

module Shapes where

import Data.Maybe (listToMaybe)

import Vectors
import DifferentialGeometry
import Rays
import qualified AABBs as BB


collideSphere :: a -> Float -> Vec3 -> Collider a
collideSphere what r s ray@(Ray {x0, u, tMax}) =
  let d = x0 - s
      a = u .* u
      b = 2 * d .* u
      c = d .* d - r * r
      delta = b * b - 4 * a * c
      ts = if delta < 0 then [] else [ (- b - sqrt delta) / (2 * a)
                                     , (- b + sqrt delta) / (2 * a) ]
  in  listToMaybe [Hit t
                       DifferentialGeometry {
                         dgPoint = x,
                         dgNormal = norm $ x - s,
                         dgDPDU = (norm (x - s)) `cross` (Vec3 0 0 1),
                         dgDPDV = (norm (x - s)) `cross` (Vec3 0 1 0)}
                       what
                  | t <- ts, t > 0, t <= tMax, let x = rayAt ray t]


boundSphere :: Float -> Vec3 -> BB.BoundingBox
boundSphere r s = BB.fromPoints (s + vof r) (s - vof r)


collideTriangle :: a -> Vec3 -> Vec3 -> Vec3 -> Collider a
collideTriangle what p0 p1 p2 (Ray {x0, u, tMax}) =
  let e1 = p1 - p0
      e2 = p2 - p0
      s = x0 - p0
      s1 = u `cross` e2
      s2 = s `cross` e1
      invDiv = 1 / (s1 .* e1)
      t = s2 .* e2 * invDiv
      b1 = s1 .* s * invDiv
      b2 = s2 .* u * invDiv
      b0 = 1 - b1 - b2
      dg = DifferentialGeometry {
             dgPoint = x0 + t @* u,
             dgNormal = norm $ e2 `cross` e1,
             dgDPDU = norm $ e2,
             dgDPDV = norm $ e1 }
  in  if   b0 >= 0 && b0 <= 1 && b1 >= 0 && b1 <= 1 && b2 >= 0 && b2 <= 1 &&
             t > 0 && t <= tMax
      then Just (Hit t dg what)
      else Nothing


boundTriangle :: Vec3 -> Vec3 -> Vec3 -> BB.BoundingBox
boundTriangle p0 p1 p2 = BB.includePoint (BB.fromPoints p0 p1) p2
