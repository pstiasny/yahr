{-# LANGUAGE NamedFieldPuns #-}

import Control.Concurrent (getNumCapabilities)
import Control.DeepSeq (force)
import Control.Monad (forM_)
import Control.Parallel.Strategies (Eval, rpar, runEval)
import Data.Functor.Identity (Identity, runIdentity)
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
import Sampling (lineBatches, squareBatches)


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

type SampleCoordinates = (Int, Int)
type Sample = (SampleCoordinates, Spectrum)

render :: Int -> Int -> [[SampleCoordinates]] ->
          (Float -> Float -> Ray) -> (Ray -> Spectrum) ->
          [Sample]
render w h batches cast li = concat $ map (map sample) batches
  where
    sample (u, v) = ((u, v), li $ cast (fromIntegral u) (fromIntegral v))

renderEval :: Int -> Int -> [[SampleCoordinates]] ->
              (Float -> Float -> Ray) -> (Ray -> Spectrum) ->
              [Sample]
renderEval w h batches cast li =
  let batch coords = [ ((u, v), li (cast (fromIntegral u) (fromIntegral v)))
                     | (u, v) <- coords ]
      evals :: [Eval [Sample]]
      evals = [ rpar (force (batch coords)) | coords <- batches ]
  in  concat $ runEval $ sequence evals

samplesToImage :: Int -> Int -> [Sample] -> Image PixelRGBF
samplesToImage w h samples =
  let uvToIndex u v = w * v + u
      image = V.create $ do
        img <- MV.new (w * h)
        forM_ samples $ \((u, v), spectrum) ->
          MV.write img (uvToIndex u v) spectrum
        return img
      getPixel u v = spectrumToPixel $ image V.! (uvToIndex u v)
  in  generateImage getPixel w h

roundUpPow2 :: Int -> Int
roundUpPow2 x = 2 ^ (ceiling (logBase 2 (fromIntegral x)))

main = do
  numThreads <- getNumCapabilities
  args <- getArgs
  if length args < 2
    then
      putStrLn "usage: yahr <input file> <output file>"
    else do
      let fileNames (i:o:[]) = (i, o)
          fileNames (_:t) = fileNames t
          (sceneFileName, outputFileName) = fileNames args
          parallelOn = "--parallel-eval" `elem` args

      sceneFile <- readFile sceneFileName

      let scene = read sceneFile :: S.Scene
          collider = cull (S.cullingMode scene) (buildCollisionModel scene)

          camera = S.camera scene
          caster = computeInitialRay camera
          width = floor $ imW camera
          height = floor $ imH camera

          lights = [v | S.PointLight v <- S.lights scene]

          li :: Ray -> Spectrum
          li = radiance (S.integrator scene) lights collider

          nBatches = roundUpPow2 $ max
            (32 * numThreads)
            width * height `div` (16 * 16)
          samplePoints = squareBatches width height nBatches

          samples =
            if parallelOn
              then renderEval width height samplePoints caster li
              else render width height samplePoints caster li
          img = samplesToImage width height samples

      putStrLn $ (show numThreads) ++ " threads, " ++ (show nBatches) ++ " batches, parallel " ++ (show parallelOn)
      savePngImage outputFileName (ImageRGBF img)
