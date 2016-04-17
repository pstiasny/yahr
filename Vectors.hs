module Vectors where

data Vec3 = Vec3 {-# UNPACK #-}!Float {-# UNPACK #-}!Float {-# UNPACK #-}!Float
            deriving (Read, Show)

type Color = Vec3
type Light = Vec3
type Normal = Vec3

vmap :: (Float -> Float) -> Vec3 -> Vec3
vmap f (Vec3 x y z) = Vec3 (f x) (f y) (f z)

vzip :: (Float -> Float -> Float) -> Vec3 -> Vec3 -> Vec3
vzip f (Vec3 ax ay az) (Vec3 bx by bz) = Vec3 (f ax bx) (f ay by) (f az bz)

vof :: Float -> Vec3
vof l = Vec3 l l l

instance Num Vec3 where
  (+) = vzip (+)
  (-) = vzip (-)
  (*) = vzip (*)
  negate = vmap negate
  abs = vmap abs
  signum = vmap signum
  fromInteger i = vof (fromInteger i)

(.*) :: Vec3 -> Vec3 -> Float
(Vec3 x1 x2 x3) .* (Vec3 y1 y2 y3) = x1 * y1 + x2 * y2 + x3 * y3
infixl 7 .*

lensq :: Vec3 -> Float
lensq v = v .* v

len :: Vec3 -> Float
len v = sqrt $ lensq v

(@*) :: Float -> Vec3 -> Vec3
f @* v = (vof f) * v
infixl 7 @*

norm :: Vec3 -> Vec3
norm v = (1 / len v) @* v

cross :: Vec3 -> Vec3 -> Vec3
(Vec3 ax ay az) `cross` (Vec3 bx by bz) =
  Vec3 (ay * bz - az * by)
       (az * bx - ax * bz)
       (ax * by - ay * bx)

data Dimension = X | Y | Z deriving (Show)

getDimension :: Dimension -> Vec3 -> Float
getDimension X (Vec3 x _ _) = x
getDimension Y (Vec3 _ y _) = y
getDimension Z (Vec3 _ _ z) = z

maxDimension :: Vec3 -> Dimension
maxDimension (Vec3 x y z)
  | x > y && x > z = X
  | y > z = Y
  | otherwise = Z
