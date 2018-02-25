{-# LANGUAGE NamedFieldPuns #-}

module Integrators where

import Linear.V3 (V3 (V3))

import Vectors
import DifferentialGeometry (DifferentialGeometry (DifferentialGeometry, dgPoint, dgNormal))
import Rays (Collider, Hit (Hit), Ray (Ray, x0, u, tMax))
import Shaders (Material, shader)
import Lights
    ( Light
    , illuminationAtPoint
    , lightSampleDir
    , lightSampleIntensity
    , lightSampleUnoccluded
    )
import qualified BSDF

data IntegratorSpec =
  WhittedIntegrator { recursionDepth :: Int }
  deriving (Read, Show)

radiance :: IntegratorSpec -> [Light] -> Collider Material -> Ray -> Spectrum
radiance spec lights rootCollider ray = vcast (recursionDepth spec) ray
  where
    vcast :: Int -> Ray -> Vec3
    vcast 0 _ = V3 0 0 0
    vcast maxDepth ray =
      case rootCollider ray of
        Nothing -> V3 0 0 0
        Just hit -> vhit maxDepth ray hit

    vhit maxDepth
         ray
         hit@(Hit tHit
                  dg@DifferentialGeometry { dgPoint = x, dgNormal = n }
                  _) =
      (n .* r) @* f r * rs + directIllumination rootCollider dg ray lights bsdf
      where bsdf = shader ray hit
            f omegai = BSDF.at bsdf dg omegai (negate (u ray))
            r = reflectionDir (u ray) n
            rs = vcast
                    (maxDepth - 1)
                    Ray { x0 = x + 0.001 @* r, u = r, tMax = 1e6 }


reflectionDir :: Vec3 -> Vec3 -> Vec3
reflectionDir u n = u - 2 * (u .* n) @* n


directIllumination :: Collider a -> DifferentialGeometry -> Ray -> [Light] -> BSDF.BSDF -> Spectrum
directIllumination rc dg ray lights bsdf = sum (map lightContribution lights)
  where
    x = dgPoint dg
    n = dgNormal dg
    lightContribution light =
      let ls = illuminationAtPoint light rc x
          lightDir = lightSampleDir ls
          k = BSDF.at bsdf dg lightDir (negate (u ray))
      in  if lensq k > 0 && lightSampleUnoccluded ls
            then abs (lightDir .* n) @* k * (lightSampleIntensity ls)
            else vof 0
