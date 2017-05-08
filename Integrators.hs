{-# LANGUAGE NamedFieldPuns #-}

module Integrators where

import Data.Maybe (isNothing)

import Vectors
import DifferentialGeometry
import Rays
import Shaders
import BSDF

data IntegratorSpec =
  WhittedIntegrator { recursionDepth :: Int }
  deriving (Read, Show)

radiance :: IntegratorSpec -> [Light] -> Collider Material -> Ray -> Spectrum
radiance spec lights rootCollider ray = vcast (recursionDepth spec) ray
  where
    vcast :: Int -> Ray -> Vec3
    vcast 0 _ = Vec3 0 0 0
    vcast maxDepth ray =
      case rootCollider ray of
        Nothing -> Vec3 0 0 0
        Just hit -> vhit maxDepth ray hit

    vhit maxDepth
         ray
         hit@(Hit tHit
                  dg@DifferentialGeometry { dgPoint = x, dgNormal = n }
                  (Material shader)) =
      (n .* r) @* f r * rs + directIllumination rootCollider dg ray lights bsdf
      where bsdf = shader ray hit
            f omegai = BSDF.at bsdf dg omegai (negate (u ray))
            r = reflectionDir (u ray) n
            rs = vcast
                    (maxDepth - 1)
                    Ray { x0 = x + 0.001 @* r, u = r, tMax = 1e6 }


reflectionDir :: Vec3 -> Vec3 -> Vec3
reflectionDir u n = u - 2 * (u .* n) @* n


reachable :: Collider a -> Vec3 -> Vec3 -> Bool
reachable rc p0 p1 = isNothing (rc probe)
  where probe = Ray { x0 = p0
                    , u = norm $ p1 - p0
                    , tMax = len (p1 - p0)
                    }


directIllumination :: Collider a -> DifferentialGeometry -> Ray -> [Light] -> BSDF.BSDF -> Spectrum
directIllumination rc dg ray lights bsdf = sum (map lightContribution lights)
  where
    x = dgPoint dg
    n = dgNormal dg
    lightContribution lightPos =
      let lightDir = norm $ lightPos - x
          k = BSDF.at bsdf dg lightDir (negate (u ray))
      in  if lensq k > 0 && reachable rc (x + 0.001 @* lightDir) lightPos
            then abs (lightDir .* n) @* k
            else vof 0
