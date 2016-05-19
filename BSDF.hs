module BSDF (BSDF (Blinn, Lambertian), at, sample, add, scale, (&*), (&+)) where

import Vectors
import DifferentialGeometry

data BSDF =
  Lambertian |
  Blinn Float |
  Scaled Spectrum BSDF |
  Composite [BSDF]

at :: BSDF -> DifferentialGeometry -> Vec3 -> Vec3 -> Spectrum
at bsdf dg i o = locAt bsdf (worldToLoc i) (worldToLoc o)
  where
    locAt (Lambertian) i o
      | cosTheta i > 0 && cosTheta o > 0 = vof (1 / pi)
      | otherwise = vof 0
    locAt (Blinn exponent) i o
      | cosTheta i > 0 && cosTheta o > 0 =
          let h = norm (i + o)
              cosThetaO = absCosTheta o
              cosThetaI = absCosTheta i
              cosThetaH = i .* h
              oDotH = o .* h
              d = (exponent+2) * (absCosTheta h ** exponent) / (2*pi)
              g = min 1 $ min (2 * cosThetaH * cosThetaO / oDotH)
                              (2 * cosThetaH * cosThetaI / oDotH)
              f = fresnel cosThetaH
          in  vof $ d * g * f / (4 * cosThetaI * cosThetaO)
      | otherwise = vof 0
    locAt (Scaled s bsdf) i o = s * locAt bsdf i o
    locAt (Composite bsdfs) i o = sum $ map (\bdsf -> locAt bdsf i o) bsdfs

    fresnel ct = 1 -- TODO

    nn@(Vec3 nnx nny nnz) = dgNormal dg
    sn@(Vec3 snx sny snz) = norm $ dgDPDU dg
    tn@(Vec3 tnx tny tnz) = nn `cross` sn

    worldToLoc v = Vec3 (v .* sn) (v .* tn) (v .* nn)
    locToWorld (Vec3 vx vy vz) = Vec3 (snx * vx + tnx * vy + nnx * vz)
                                      (sny * vx + tny * vy + nny * vz)
                                      (snz * vx + tnz * vy + nnz * vz)

    cosTheta (Vec3 _ _ z) = z
    absCosTheta (Vec3 _ _ z) = abs z

sample :: BSDF -> Vec3 -> Vec3 -> [Vec3]
sample _ _ _ = []

scale :: Spectrum -> BSDF -> BSDF
scale s' (Scaled s wrapped) = Scaled (s' * s) wrapped
scale s wrap = Scaled s wrap

add :: BSDF -> BSDF -> BSDF
add (Composite xs) (Composite ys) = Composite (xs ++ ys)
add (Composite xs) x = Composite (x:xs)
add x (Composite xs) = Composite (x:xs)
add x y = Composite ([x, y])

(&*) :: Spectrum -> BSDF -> BSDF
(&*) = scale
infixl 7 &*

(&+) :: BSDF -> BSDF -> BSDF
(&+) = add
infixl 6 &+
