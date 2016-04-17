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
import qualified AABBs as BB


bvh :: [(BB.BoundingBox, Collider a)] -> Collider a
bvh [] = const Nothing
bvh xs = snd $ buildTree xs
  where
    buildTree :: [(BB.BoundingBox, Collider a)] -> (BB.BoundingBox, Collider a)
    buildTree [(bb, x)] = (bb, BB.wrapCollider x bb)
    buildTree xs = (jointBB, BB.wrapCollider (collideAll [x1, x2]) jointBB)
      where jointBB = BB.join bb1 bb2
            (bb1, x1) = buildTree xs1
            (bb2, x2) = buildTree xs2
            (xs1, xs2) = splitAt (length xs `div` 2) xs


collideScene :: S.Scene -> Collider Shader
collideScene s = bvh $ zip sceneObjBounds sceneColliders
  where
    sceneObjects = S.objects s >>= S.expand

    sceneColliders = map collideSceneObject sceneObjects
    collideSceneObject (S.Sphere p r mId) = collideSphere (sh mId) r p
    collideSceneObject (S.Plane p n mId) = collidePlane (sh mId) p n
    collideSceneObject (S.Triangle p0 p1 p2 mId) = collideTriangle (sh mId) p0 p1 p2

    sceneObjBounds = map boundSceneObject sceneObjects
    boundSceneObject (S.Sphere p r mId) = boundSphere r p
    boundSceneObject (S.Triangle p0 p1 p2 mId) = boundTriangle p0 p1 p2

    sh mId = mats ! mId
    mats :: Map String Shader
    mats = fromList [(S.id m, shaderForMat m) | m <- S.materials s]
    shaderForMat mat = case mat of
      S.BlinnPhongMaterial id ambient diffuse specular shininess ->
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
                               Ray {x0 = point + 0.001 @* n, u = n}
            visibleLights = filter isReachable lights
            isReachable light =
              let rayToLight = Ray {x0 = point + 0.001 @* hitNormal,
                                    u = norm $ light - point}
                  mhit = collide rayToLight
                  lightDistSq = lensq (light - point)
              in  case mhit of
                    Just (Hit {point = lp}) -> lensq (lp - point) > lightDistSq
                    Nothing -> True


getPixel :: Collider Shader -> [Light] -> (Float -> Float -> Ray) -> Int -> Int -> PixelRGBF
getPixel collide lights cast x y =
  let ray = cast (fromIntegral x) (fromIntegral y)
      Vec3 r g b = vcast 3 collide lights ray
  in  PixelRGBF r g b


main = do
  args <- getArgs
  if length args /= 2
    then
      putStrLn "usage: yahr <input file> <output file>"
    else do
      sceneFile <- readFile $ args !! 0

      let scene = read sceneFile :: S.Scene
          collider = collideScene scene

          camera = S.camera scene
          caster = computeInitialRay camera
          width = floor $ imW camera
          height = floor $ imH camera

          lights = [v | S.PointLight v <- S.lights scene]

          img = generateImage (getPixel collider lights caster) width height
      savePngImage (args !! 1) (ImageRGBF img)
