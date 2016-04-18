{-# LANGUAGE NamedFieldPuns #-}
module Rays where

import Data.List (sortOn)
import Data.Maybe (catMaybes, listToMaybe)

import Vectors

-- Ray described by l(t) = x0 + t * u
data Ray = Ray { x0 :: Vec3, u :: Vec3, tMax :: Float } deriving (Show)

data Hit a = Hit { rayT :: Float, point :: Vec3, normal :: Vec3, what :: a } deriving Show

type Collider a = Ray -> Maybe (Hit a)

nullCollider = const Nothing

rayAt (Ray { x0, u }) t = x0 + t @* u

cutRay newMaxT r = r { tMax = newMaxT }

collideAll :: [Collider a] -> Collider a
collideAll colliders ray =
  fst $ foldl collideWith (Nothing, ray) colliders
  where
    collideWith (mhit, ray) collider =
      case collider ray of
        Just (h@Hit { rayT }) -> (Just h, cutRay rayT ray)
        Nothing -> (mhit, ray)
