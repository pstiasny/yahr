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

radiance :: (Monad m) => IntegratorSpec -> [Light] -> Collider Material -> Ray -> m Spectrum
radiance spec lights rootCollider ray = vcast (recursionDepth spec) ray
  where
    vcast :: (Monad m) => Int -> Ray -> m Vec3
    vcast 0 _ = return (Vec3 0 0 0)
    vcast maxDepth ray =
      case rootCollider ray of
        Nothing -> return (Vec3 0 0 0)
        Just hit@(Hit tHit
                      dg@DifferentialGeometry { dgPoint = point,
                                                dgNormal = hitNormal }
                      (Material shader)) ->
          do
            next <- castNext reflectionDir
            return $ (hitNormal .* reflectionDir) @* f reflectionDir * next +
              sum (map lightContribution lights)
          where bsdf = shader ray hit
                f omegai = BSDF.at bsdf dg omegai (negate (u ray))
                lightContribution lightPos =
                  let lightDir = norm $ lightPos - point
                      k = f lightDir
                      reachable = isReachable lightPos
                  in  if lensq k > 0 && reachable
                        then abs (lightDir .* hitNormal) @* k -- TODO: light spectrum, intensity
                        else vof 0
                castNext n = vcast (maxDepth - 1)
                                   Ray {x0 = point + 0.001 @* n, u = n, tMax = 1e6}
                isReachable light =
                  let rayToLight = Ray {x0 = point + 0.001 @* hitNormal,
                                        u = norm $ light - point,
                                        tMax = lightDist}
                      mhit = rootCollider rayToLight
                      lightDist = len (light - point)
                  in  isNothing mhit
                reflectionDir = (u ray) - 2 * ((u ray) .* hitNormal) @* hitNormal
