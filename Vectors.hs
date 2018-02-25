module Vectors where

import Control.Monad.Zip (mzipWith)
import qualified Linear as L
import qualified Linear.Metric as LM
import qualified Linear.Vector as LV
import qualified Linear.V3 as LV3
import Linear.V3 (V3 (V3))

type Vec3 = L.V3 Float

type Spectrum = Vec3
type Normal = Vec3

vmap :: (Float -> Float) -> Vec3 -> Vec3
vmap = fmap

vzip :: (Float -> Float -> Float) -> Vec3 -> Vec3 -> Vec3
vzip = mzipWith

vof :: Float -> Vec3
vof l = V3 l l l

(.*) :: Vec3 -> Vec3 -> Float
(.*) = LM.dot
infixl 7 .*

lensq :: Vec3 -> Float
lensq = LM.quadrance

len :: Vec3 -> Float
len = LM.norm

(@*) :: Float -> Vec3 -> Vec3
(@*) = (LV.*^)
infixl 7 @*

norm :: Vec3 -> Vec3
norm = LM.normalize

cross :: Vec3 -> Vec3 -> Vec3
cross = LV3.cross

data Dimension = X | Y | Z deriving (Show)

getDimension :: Dimension -> Vec3 -> Float
getDimension X (V3 x _ _) = x
getDimension Y (V3 _ y _) = y
getDimension Z (V3 _ _ z) = z

maxDimension :: Vec3 -> Dimension
maxDimension (V3 x y z)
  | x > y && x > z = X
  | y > z = Y
  | otherwise = Z
