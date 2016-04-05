module Rays where

import Vectors

-- Ray described by l(t) = x0 + t * u
data Ray = Ray { x0 :: Vec3, u :: Vec3 }

data Hit a = Hit { point :: Vec3, normal :: Vec3, what :: a }

type Collider a = Ray -> Maybe (Hit a)
