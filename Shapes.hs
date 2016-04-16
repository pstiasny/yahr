{-# LANGUAGE NamedFieldPuns #-}

module Shapes where

import Data.List (sortOn)
import Data.Maybe (isJust, fromJust)

import Vectors
import Rays



headMay :: [a] -> Maybe a
headMay [] = Nothing
headMay (h:_) = Just h


collideSphere :: a -> Float -> Vec3 -> Collider a
collideSphere shader r s (Ray {x0, u}) =
  let d = x0 - s
      a = u .* u
      b = 2 * d .* u
      c = d .* d - r * r
      delta = b * b - 4 * a * c
      ts = if delta < 0 then [] else [ (- b - sqrt delta) / (2 * a)
                                     , (- b + sqrt delta) / (2 * a) ]
      xs = [ x0 + t @* u | t <- ts, t > 0 ]
  in  headMay [Hit { point = x, normal = norm $ x - s, what = shader }
              | x <- xs]


collidePlane :: a -> Vec3 -> Vec3 -> Collider a
collidePlane shader origin normal (Ray {x0, u}) =
  let t = (normal .* (origin - x0)) / (normal .* u)
  in  if   t > 0
      then Just (Hit { point = x0 + t @* u, normal = normal, what = shader })
      else Nothing


collideTriangle :: a -> Vec3 -> Vec3 -> Vec3 -> Collider a
collideTriangle shader p0 p1 p2 (Ray {x0, u}) =
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
  in  if   b0 >= 0 && b0 <= 1 && b1 >= 0 && b1 <= 1 && b2 >= 0 && b2 <= 1 && t > 0
      then Just (Hit { point = x0 + t @* u, normal = norm $ e2 `cross` e1,
                       what = shader })
      else Nothing


collideAll :: [Collider a] -> Collider a
collideAll colliders ray =
  headMay $
    sortOn (lensq . (x0 ray -) . point) $
    map fromJust $
    filter isJust $
    map ($ ray) colliders


