module DifferentialGeometry where

import Vectors

data DifferentialGeometry =
  DifferentialGeometry {
    dgPoint :: Vec3,
    dgNormal :: Vec3,
    dgDPDU :: Vec3, dgDPDV :: Vec3 }
  deriving Show
