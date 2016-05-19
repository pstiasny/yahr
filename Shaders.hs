{-# LANGUAGE NamedFieldPuns #-}

module Shaders where

import Vectors
import Rays
import BSDF

type Shader = Ray -> Hit Material -> BSDF.BSDF
data Material = Material Shader

blinnPhong :: Vec3 -> Vec3 -> Vec3 -> Float -> Shader
blinnPhong ambientColor diffuseColor specularColor shininess ray hit =
  diffuseColor &* Lambertian &+ specularColor &* BSDF.Blinn shininess
