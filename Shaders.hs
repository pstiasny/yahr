{-# LANGUAGE NamedFieldPuns #-}

module Shaders where

import Vectors
import Rays


data ShaderInput = ShaderInput { ray :: Ray,
                                 hit :: Hit Shader,
                                 lights :: [Light],
                                 cast :: Normal -> Color
                               }

type Shader = ShaderInput -> Color

type VHit = Hit Shader


blinnPhong :: Vec3 -> Vec3 -> Vec3 -> Float -> Shader
blinnPhong ambientColor diffuseColor specularColor shininess
           ShaderInput { cast, ray, hit, lights } =
  let viewDir = negate $ u ray
      normalDir = normal hit
      reflectionDir = (u ray) - 2 * ((u ray) .* normalDir) @* normalDir

      pointLight light =
        let lightDir = norm $ light - (point hit)
            lambertian = lightDir .* normalDir
            h = norm $ lightDir + viewDir
            specularAngle = max 0 $ h .* normalDir
            specularOut = specularAngle ** shininess
        in  if   lambertian > 0
            then lambertian @* diffuseColor + specularOut @* specularColor
            else unitv 0

  in  ambientColor + (sum $ map pointLight lights) + 0.4 @* cast reflectionDir
