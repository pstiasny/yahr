{-# LANGUAGE NamedFieldPuns #-}

import Data.Map (Map, fromList, (!))
import System.Environment
import Codec.Picture

import Vectors
import Rays
import qualified Scene
import Scene hiding (lights)
import Shapes
import Cameras
import Shaders



collideScene :: Scene -> Collider Shader
collideScene s = collideAll sceneColliders
  where
    sceneColliders = [collideSceneObject o | o <- objects s]
    collideSceneObject (Sphere p r mId) = collideSphere (sh mId) r p
    collideSceneObject (Plane p n mId) = collidePlane (sh mId) p n
    sh mId = mats ! mId
    mats :: Map String Shader
    mats = fromList [(Scene.id m, shaderForMat m) | m <- materials s]
    shaderForMat mat = case mat of
      BlinnPhongMaterial id ambient diffuse specular shininess ->
        blinnPhong ambient diffuse specular shininess


vcast :: Int -> Collider Shader -> [Light] -> Ray -> Color
vcast 0 _ _ _ = (Vec3 0 0 0)
vcast maxDepth collide lights ray =
  case collide ray of
    Nothing -> Vec3 0 0 0
    Just hit@Hit {point, normal = hitNormal, what = shader} ->
      shader (ShaderInput { ray = ray,
                            hit = hit,
                            lights = visibleLights,
                            cast = castNext })
      where castNext n = vcast (maxDepth - 1)
                               collide
                               lights
                               Ray { x0 = point + 0.001 @* n, u = n }
            visibleLights = filter isReachable lights
            isReachable light =
              let rayToLight = Ray {x0 = point + 0.001 @* hitNormal, u = norm $ light - point}
                  mhit = collide rayToLight
                  lightDistSq = lensq (light - point)
              in  case mhit of
                    Just (Hit {point = lp}) -> lensq (lp - point) > lightDistSq
                    Nothing -> True

    
getPixel :: Collider Shader -> [Light] -> Camera -> Int -> Int -> PixelRGBF
getPixel collide lights cam x y = 
  let ray = computeInitialRay cam x y
      Vec3 r g b = vcast 5 collide lights ray
  in  PixelRGBF r g b


main = do
  args <- getArgs
  if length args /= 2
    then
      putStrLn "usage: yahr <input file> <output file>"
    else do
      sceneFile <- readFile $ args !! 0

      let scene = read sceneFile :: Scene
          collider = collideScene scene

          focalLength = 1
          camera = Camera { imW = 800, imH = 600,
                            w = 800/600, h = 1, l = focalLength }
          lights = [v | PointLight v <- Scene.lights scene]
          img = generateImage (getPixel collider lights camera) 800 600
      savePngImage (args !! 1) (ImageRGBF img)
