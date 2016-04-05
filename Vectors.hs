module Vectors where

data Vec3 = Vec3 Float Float Float deriving (Read, Show)
instance Num Vec3 where
  (Vec3 x1 x2 x3) + (Vec3 y1 y2 y3) = Vec3 (x1 + y1) (x2 + y2) (x3 + y3)
  (Vec3 x1 x2 x3) - (Vec3 y1 y2 y3) = Vec3 (x1 - y1) (x2 - y2) (x3 - y3)
  (Vec3 x1 x2 x3) * (Vec3 y1 y2 y3) = Vec3 (x1 * y1) (x2 * y2) (x3 * y3)
  negate (Vec3 x1 x2 x3) = Vec3 (negate x1) (negate x2) (negate x3)
  abs (Vec3 x1 x2 x3) = Vec3 (abs x1) (abs x2) (abs x3)
  signum (Vec3 x1 x2 x3) = Vec3 (signum x1) (signum x2) (signum x3)
  fromInteger i = Vec3 (fromInteger i) (fromInteger i) (fromInteger i)

unitv :: Float -> Vec3
unitv l = Vec3 l l l

(.*) :: Vec3 -> Vec3 -> Float
(Vec3 x1 x2 x3) .* (Vec3 y1 y2 y3) = x1 * y1 + x2 * y2 + x3 * y3
infixl 7 .*

lensq :: Vec3 -> Float
lensq v = v .* v

len :: Vec3 -> Float
len v = sqrt $ lensq v

norm :: Vec3 -> Vec3
norm v = v * unitv (1 / len v)

(@*) :: Float -> Vec3 -> Vec3
f @* v = (unitv f) * v
infixl 7 @*

type Color = Vec3
type Light = Vec3
type Normal = Vec3
