{-# LANGUAGE NamedFieldPuns #-}

module Cameras (
    Camera (Camera),
    computeInitialRay,
    focalLength,
    imH,
    imW,
    lookDir,
    position,
    upDir
    ) where

import qualified Linear as L
import Linear ((!*), (!*!))

import Vectors
import Rays


type TransfMx = L.M44 Float


fromBasis :: Vec3 -> Vec3 -> Vec3 -> TransfMx
fromBasis (Vec3 ax ay az) (Vec3 bx by bz) (Vec3 cx cy cz) =
  L.V4 (L.V4 ax bx cx 0)
       (L.V4 ay by cy 0)
       (L.V4 az bz cz 0)
       (L.V4 0  0  0  1)


translate :: Vec3 -> TransfMx
translate (Vec3 tx ty tz) =
  L.V4 (L.V4 1 0 0 tx)
       (L.V4 0 1 0 ty)
       (L.V4 0 0 1 tz)
       (L.V4 0 0 0 1 )


transformPoint :: TransfMx -> Vec3 -> Vec3
transformPoint tf (Vec3 x y z) =
  let tv = tf !* L.V4 x y z 1
      L.V3 tx ty tz = L.normalizePoint tv
  in  Vec3 tx ty tz


transformVector :: TransfMx -> Vec3 -> Vec3
transformVector tf (Vec3 x y z) =
  let tv = tf !* L.V4 x y z 0
      L.V4 tx ty tz _ = tv
  in  Vec3 tx ty tz


data Camera = Camera { imW :: Float, imH :: Float, focalLength :: Float,
                       lookDir :: Vec3, upDir :: Vec3, position :: Vec3
                     } deriving (Show, Read)


cameraToWorld :: Vec3 -> Vec3 -> Vec3 -> TransfMx
cameraToWorld lookDir upDir position =
  let forward = norm lookDir
      left = norm $ forward `cross` upDir
      up = left `cross` forward
      dirTf = fromBasis (negate left) up forward
  in  translate position !*! dirTf


rasterToCamera :: Float -> Float -> TransfMx
rasterToCamera imW imH =
  L.V4 (L.V4 (frameW/imW) 0             0 (-frameW/2))
       (L.V4 0            (-frameH/imH) 0 (frameH/2) )
       (L.V4 0            0             1 0          )
       (L.V4 0            0             0 1          )
  where frameW = imW / imH
        frameH = 1


computeInitialRay :: Camera -> Float -> Float -> Ray
computeInitialRay (Camera {imW, imH, focalLength, lookDir, upDir, position}) =
  let tf = cameraToWorld lookDir upDir position
      vtf = tf !*! rasterToCamera imW imH

  in  \u v ->
    let origin = transformPoint tf (Vec3 0 0 0)
        direction = transformPoint vtf (Vec3 u v focalLength) - origin
    in  Ray { x0 = origin, u = norm $ direction }
