module Rays where

import Data.List (sortOn)
import Data.Maybe (catMaybes, listToMaybe)

import Vectors

-- Ray described by l(t) = x0 + t * u
data Ray = Ray { x0 :: Vec3, u :: Vec3 } deriving (Show)

data Hit a = Hit { point :: Vec3, normal :: Vec3, what :: a } deriving Show

type Collider a = Ray -> Maybe (Hit a)


collideAll :: [Collider a] -> Collider a
collideAll colliders ray =
  listToMaybe $
    sortOn (lensq . (x0 ray -) . point) $
    catMaybes $
    map ($ ray) colliders
