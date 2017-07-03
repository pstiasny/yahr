module Lights where

import Vectors
import Rays (Collider, reachable)


data Light = PointLight Vec3 Spectrum deriving (Read, Show)
data LightSample = LightSample
  { lightSampleDir :: Vec3
  , lightSampleIntensity :: Spectrum
  , lightSampleUnoccluded :: Bool
  }


illuminationAtPoint :: Light -> Collider a -> Vec3 -> LightSample
illuminationAtPoint (PointLight lightPos spectrum) rc x =
  let pointToLight = lightPos - x
      lightDir = norm $ pointToLight
      intensity = (1 / (lensq pointToLight)) @* spectrum
      unoccluded = reachable rc (x + 0.001 @* lightDir) lightPos
  in  LightSample { lightSampleDir = lightDir
                  , lightSampleIntensity = intensity
                  , lightSampleUnoccluded = unoccluded
                  }
