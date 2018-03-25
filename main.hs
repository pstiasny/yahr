{-# LANGUAGE NamedFieldPuns #-}

import Data.Semigroup ((<>))
import Text.Printf (printf)
import System.IO
import System.CPUTime (getCPUTime)
import System.Exit (exitSuccess)
import Control.Exception (bracket, evaluate)
import Control.Concurrent (getNumCapabilities)
import Control.DeepSeq (rnf, force)
import Control.Monad (forM_)
import Control.Parallel.Strategies (Eval, rpar, runEval)
import Control.Monad.Par (runPar, spawn, get)
import Data.Map (Map, fromList, empty, insert, (!))
import Data.Tuple (swap)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import System.Environment
import Codec.Picture
import qualified Data.Attoparsec.ByteString as AP
import qualified Data.ByteString.Char8 as B
import Linear.V3 (V3 (V3))
import qualified Options.Applicative as OA

import Vectors
import Rays
import qualified Scene as S
import Shapes
import Cameras
import Shaders
import Culling (cull)
import AABBs (BoundingBox)
import Integrators (radiance)
import Sampling (lineBatches, squareBatches)


data Invocation = Invocation
  { invInput :: String
  , invOutput :: String
  , invParMode :: String
  }


invocationParser = Invocation
  <$> OA.argument OA.str (OA.metavar "input")
  <*> OA.argument OA.str (OA.metavar "output")
  <*> OA.strOption (OA.long "parallel-mode" <> OA.value "sequential")


buildCollisionModel :: InterpreterState -> S.Scene -> [(BoundingBox, Collider Material)]
buildCollisionModel ist s = map makeModel triangles
  where
    materials = fromList [(materialId m, m) | m <- S.materials s]
    triangles = S.objects s >>= S.expand materials (istMeshes ist)
    makeModel t = (boundTriangle t, collideTriangle (S.tMaterial t) t)

spectrumToPixel :: Spectrum -> PixelRGBF
spectrumToPixel (V3 r g b) = PixelRGBF r g b

type SampleCoordinates = (Int, Int)
type Sample = (SampleCoordinates, Spectrum)

renderSeq :: Int -> Int -> [[SampleCoordinates]] ->
             (Float -> Float -> Ray) -> (Ray -> Spectrum) ->
             [Sample]
renderSeq w h batches cast li = concat $ map (map sample) batches
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


renderPar :: Int -> Int -> [[SampleCoordinates]] ->
             (Float -> Float -> Ray) -> (Ray -> Spectrum) ->
             [Sample]
renderPar w h batches cast li =
  let batch coords = [ ((u, v), li (cast (fromIntegral u) (fromIntegral v)))
                     | (u, v) <- coords ]
      par = do
        ivars <- mapM (spawn . return . batch) batches
        pixelBatches <- mapM get ivars
        return $ concat pixelBatches
  in  runPar par

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

data InterpreterState = InterpreterState
  { istInvocation :: Invocation
  , istMeshes :: Map String S.Mesh
  }


initialState invocation = InterpreterState
  { istInvocation = invocation
  , istMeshes = empty
  }

handleCommand :: S.Command -> InterpreterState -> IO InterpreterState

handleCommand (S.MeshCommand mesh) state = do
  rnf mesh `seq` return ()
  printf "Mesh loaded: %s\n" (S.meshId mesh)
  return $ state
    { istMeshes = insert (S.meshId mesh) mesh (istMeshes state) }

handleCommand (S.SceneCommand scene) state = do
  let invocation = istInvocation state
  numThreads <- getNumCapabilities

  t0 <- getCPUTime
  putStrLn "Building internal representation"
  let collisionModel = buildCollisionModel state scene
  rnf collisionModel `seq` return ()
  tMdl <- getCPUTime
  printf "%0.9f seconds\n" ((fromIntegral (tMdl - t0)) / (10^12) :: Double)

  putStrLn "Building acceleration data structures"
  let collider = cull (S.cullingMode scene) collisionModel
  rnf collider `seq` return ()
  tCull <- getCPUTime
  printf "%0.9f seconds\n" ((fromIntegral (tCull - tMdl)) / (10^12) :: Double)

  let camera = S.camera scene
      caster = computeInitialRay camera
      width = floor $ imW camera
      height = floor $ imH camera

      li :: Ray -> Spectrum
      li = radiance (S.integrator scene) (S.lights scene) collider

      nBatches = roundUpPow2 $ max
        (32 * numThreads)
        (width * height `div` (16 * 16))
      {-samplePoints = squareBatches width height nBatches-}
      samplePoints = lineBatches width height

      samples =
        case (invParMode invocation) of
          "sequential" -> renderSeq width height samplePoints caster li
          "eval" -> renderEval width height samplePoints caster li
          "par" -> renderPar width height samplePoints caster li
      img = samplesToImage width height samples

  putStrLn $ (show numThreads) ++ " threads, " ++ (show nBatches) ++
             " batches, parallel " ++ (invParMode invocation)
  savePngImage (invOutput invocation) (ImageRGBF img)

  return state

handleCommand (S.EndCommand) state = do
  exitSuccess
  return state

main = do
  invocation <- OA.execParser (OA.info invocationParser OA.fullDesc)
  bracket
    (openFile (invInput invocation) ReadMode)
    hClose
    $ \fileHandle -> readLoop fileHandle (initialState invocation) B.empty
  where readLoop fileHandle state initialInput = do
          parseRes <- AP.parseWith (moreInput fileHandle) S.input initialInput
          case parseRes of
            AP.Fail _ ctxs msg -> do
              putStrLn ("parse error: " ++ msg)
              print ctxs
            AP.Done rest command -> do
              state' <- handleCommand command state
              readLoop fileHandle state' rest
        moreInput h = B.hGet h (80*1024)
