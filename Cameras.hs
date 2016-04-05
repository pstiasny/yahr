{-# LANGUAGE NamedFieldPuns #-}

module Cameras where

import Vectors
import Rays


data Camera = Camera { imW :: Float, imH :: Float,
                       w :: Float, h :: Float, l :: Float
                     } deriving (Show)  


computeInitialRay :: Camera -> Int -> Int -> Ray
computeInitialRay (Camera {imW, imH, w, h, l}) x y =
  let fx = fromIntegral x
      fy = fromIntegral y
      px = ((fx - imW / 2) / imW) * w / 2
      py = ((imH / 2 - fy) / imH) * h / 2
  in  Ray { x0 = Vec3 0 0 (-l), u = norm $ Vec3 px py l }
