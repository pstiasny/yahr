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


collideAll :: [Collider a] -> Collider a
collideAll colliders ray =
  headMay $
    sortOn (lensq . (x0 ray -) . point) $
    map fromJust $
    filter isJust $
    map ($ ray) colliders


