{-# LANGUAGE NamedFieldPuns #-}

import Data.Map (Map, fromList, (!))
import System.Environment
import Codec.Picture

import Vectors
import Rays
import qualified Scene as S
import Shapes
import Cameras
import Shaders
import Culling
import AABBs (BoundingBox)
import Integrators (radiance)


buildCollisionModel :: S.Scene -> [(BoundingBox, Collider Material)]
buildCollisionModel s = zip sceneObjBounds sceneColliders
  where
    sceneObjects = S.objects s >>= S.expand

    sceneColliders = map collideSceneObject sceneObjects
    collideSceneObject (S.Sphere p r mId) = collideSphere (mat mId) r p
    collideSceneObject (S.Triangle p0 p1 p2 mId) = collideTriangle (mat mId) p0 p1 p2

    sceneObjBounds = map boundSceneObject sceneObjects
    boundSceneObject (S.Sphere p r mId) = boundSphere r p
    boundSceneObject (S.Triangle p0 p1 p2 mId) = boundTriangle p0 p1 p2

    mat mId = Material (mats ! mId)
    mats :: Map String Shader
    mats = fromList [(S.id m, shaderFromDescription m) | m <- S.materials s]
    shaderFromDescription desc = case desc of
      S.BlinnPhongMaterial id ambient diffuse specular shininess ->
        blinnPhong ambient diffuse specular shininess


getPixel :: (Float -> Float -> Ray) -> (Ray -> Spectrum) -> Int -> Int -> PixelRGBF
getPixel cast li x y =
  let ray = cast (fromIntegral x) (fromIntegral y)
      Vec3 r g b = li ray
  in  PixelRGBF r g b


main = do
  args <- getArgs
  if length args /= 2
    then
      putStrLn "usage: yahr <input file> <output file>"
    else do
      sceneFile <- readFile $ args !! 0

      let scene = read sceneFile :: S.Scene
          collider = cull (S.cullingMode scene) (buildCollisionModel scene)

          camera = S.camera scene
          caster = computeInitialRay camera
          width = floor $ imW camera
          height = floor $ imH camera

          lights = [v | S.PointLight v <- S.lights scene]

          li = radiance (S.integrator scene) lights collider
          img = generateImage (getPixel caster li) width height

      savePngImage (args !! 1) (ImageRGBF img)
