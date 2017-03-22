{-# LANGUAGE NamedFieldPuns #-}

import Control.Monad (forM_)
import Data.Functor.Identity (runIdentity)
import Data.Map (Map, fromList, (!))
import Data.Tuple (swap)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
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

spectrumToPixel :: Spectrum -> PixelRGBF
spectrumToPixel (Vec3 r g b) = PixelRGBF r g b

render :: Int -> Int -> [(Int, Int)] ->
          (Float -> Float -> Ray) -> (Ray -> Spectrum) ->
          Int -> Int -> PixelRGBF
render w h samples cast li =
  let uvToIndex u v = w * v + u
      image = V.create $ do
        img <- MV.new (w * h)
        forM_ samples $ \(u,v) ->
          let ray = cast (fromIntegral u) (fromIntegral v)
              spectrum = li ray
          in  MV.write img (uvToIndex u v) spectrum
        return img
  in  \u v -> spectrumToPixel $ image V.! (uvToIndex u v)

scannySamplePoints w h = [(u, v) | u <- [0..w-1], v <- [0..h-1]]

patchySamplePoints w h =
  (concat $ [shift x y $ scannySamplePoints patchW patchH
            | x <- [0..patchCountX - 1], y <- [0..patchCountY - 1]])
  ++
  (concat $ [shift patchCountX y $ scannySamplePoints lastPatchW patchH
            | y <- [0..patchCountY - 1]])
  ++
  (concat $ [shift x patchCountY $ scannySamplePoints patchW lastPatchH
            | x <- [0..patchCountX - 1]])
  ++
  (shift patchCountX patchCountY $ scannySamplePoints lastPatchW lastPatchH)

  where patchW = 20
        patchH = 20
        (patchCountX, lastPatchW) = w `quotRem` patchW
        (patchCountY, lastPatchH) = h `quotRem` patchH
        shift x y samples = [(u + x * patchW, v + y * patchH)
                            | (u, v) <- samples]

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

          li = (runIdentity . radiance (S.integrator scene) lights collider)
          {-samplePoints = patchySamplePoints width height-}
          samplePoints = scannySamplePoints width height
          sampleImg = render width height samplePoints caster li
          img = generateImage sampleImg width height

      savePngImage (args !! 1) (ImageRGBF img)
