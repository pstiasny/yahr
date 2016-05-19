{-# LANGUAGE NamedFieldPuns #-}
module Rays where

import Data.List (sortOn)
import Data.Maybe (catMaybes, listToMaybe)

import Vectors
import DifferentialGeometry

-- Ray described by l(t) = x0 + t * u
data Ray = Ray { x0 :: Vec3, u :: Vec3, tMax :: Float } deriving (Show)

{-data Hit a =-}
  {-Hit { tHit :: Float, point :: Vec3, normal :: Vec3, what :: a }-}
  {-deriving Show-}
data Hit a = Hit Float DifferentialGeometry a deriving Show

tHit :: Hit a -> Float
tHit (Hit t _ _) = t

point :: Hit a -> Vec3
point (Hit _ dg _) = dgPoint dg

normal :: Hit a -> Vec3
normal (Hit _ dg _) = dgNormal dg

what :: Hit a -> a
what (Hit _ _ a) = a


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
        Just h -> (Just h, cutRay (tHit h) ray)
        Nothing -> (mhit, ray)
