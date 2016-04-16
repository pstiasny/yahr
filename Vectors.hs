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

instance Fractional Vec3 where
  (Vec3 x1 x2 x3) / (Vec3 y1 y2 y3) = Vec3 (x1 / y1) (x2 / y2) (x3 / y3)
  recip (Vec3 x1 x2 x3) = Vec3 (recip x1) (recip x2) (recip x3)
  fromRational i = Vec3 (fromRational i) (fromRational i) (fromRational i)

unitv :: Float -> Vec3
unitv l = Vec3 l l l

(.*) :: Vec3 -> Vec3 -> Float
(Vec3 x1 x2 x3) .* (Vec3 y1 y2 y3) = x1 * y1 + x2 * y2 + x3 * y3
infixl 7 .*

lensq :: Vec3 -> Float
lensq v = v .* v

len :: Vec3 -> Float
len v = sqrt $ lensq v

(@*) :: Float -> Vec3 -> Vec3
f @* v = (unitv f) * v
infixl 7 @*

norm :: Vec3 -> Vec3
norm v = (1 / len v) @* v

cross :: Vec3 -> Vec3 -> Vec3
(Vec3 ax ay az) `cross` (Vec3 bx by bz) =
  Vec3 (ay * bz - az * by)
       (az * bx - ax * bz)
       (ax * by - ay * bx)


type Color = Vec3
type Light = Vec3
type Normal = Vec3


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
