{-# LANGUAGE NamedFieldPuns #-}

module Shaders where

import Vectors
import Rays
import BSDF

data Material = BlinnPhongMaterial { materialId :: String,
                                     ambient :: Spectrum,
                                     diffuse :: Spectrum,
                                     specular :: Spectrum,
                                     shininess :: Float
                                   } deriving (Read, Show)

type Shader = Ray -> Hit Material -> BSDF.BSDF

blinnPhong :: Shader
blinnPhong ray (Hit _ _ (BlinnPhongMaterial {ambient, diffuse, specular, shininess})) =
  diffuse &* Lambertian &+ specular &* BSDF.Blinn shininess

shader = blinnPhong
