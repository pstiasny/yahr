{-# LANGUAGE NamedFieldPuns #-}

import Data.List (sortOn)
import qualified Data.Map as Map
import System.Environment
import Codec.Picture

import Data.Vectors
import Data.Scene

data Hit a = Hit { point :: Vec3, normal :: Vec3, what :: a }

type Light = Vec3
type Normal = Vec3
data ShaderInput = ShaderInput { ray :: Ray,
                                 hit :: Hit Shader,
                                 lights :: [Light],
                                 caster :: Normal -> Color
                               }
type Shader = ShaderInput -> Color
type VHit = Hit Shader

-- Ray described by l(t) = x0 + t * u
data Ray = Ray { x0 :: Vec3, u :: Vec3 }

data Camera = Camera { imW :: Float, imH :: Float,
                       w :: Float, h :: Float, l :: Float
                     } deriving (Show)  

blinnPhong :: Vec3 -> Vec3 -> Vec3 -> Float -> [Vec3] -> Shader
blinnPhong ambientColor diffuseColor specularColor shininess
           lights ShaderInput { ray, hit } =
  let viewDir = negate $ u ray
      normalDir = normal hit

      pointLight light =
        let lightDir = norm $ light - (point hit)
            lambertian = lightDir .* normalDir
            h = norm $ lightDir + viewDir
            specularAngle = max 0 $ h .* normalDir
            specularOut = specularAngle ** shininess
        in  if   lambertian > 0
            then lambertian @* diffuseColor + specularOut @* specularColor
            else unitv 0

  in  ambientColor + (sum $ map pointLight lights)

collideSphere :: a -> Float -> Vec3 -> Ray -> [Hit a]
collideSphere shader r s (Ray {x0, u}) =
  let d = x0 - s
      a = u .* u
      b = 2 * d .* u
      c = d .* d - r * r
      delta = b * b - 4 * a * c
      ts = if delta < 0 then [] else [ (- b - sqrt delta) / (2 * a)
                                     , (- b + sqrt delta) / (2 * a) ]
      xs = [ x0 + t @* u | t <- ts ]
  in  [Hit { point = x, normal = norm $ x - s, what = shader } | x <- xs]


collideAll :: [Ray -> [Hit a]] -> Ray -> [Hit a]
collideAll colliders line = foldl1 (++) $ map ($line) colliders


collideScene :: Scene -> Ray -> [Hit Shader]
collideScene s = collideAll sceneColliders
  where
    sceneColliders = [collideSceneObject o | o <- objects s]
    collideSceneObject so = case so of
      Sphere p r mId -> collideSphere (sh mId) r p
    sh mId = mats Map.! mId
    mats = Map.fromList [(Data.Scene.id m, shaderForMat m) | m <- materials s]
    shaderForMat mat = case mat of
      BlinnPhongMaterial id ambient diffuse specular shininess ->
        blinnPhong ambient diffuse specular shininess lights
    lights = [Vec3 0 10 15]


computeInitialRay :: Camera -> Int -> Int -> Ray
computeInitialRay (Camera {imW, imH, w, h, l}) x y =
  let fx = fromIntegral x
      fy = fromIntegral y
      px = ((fx - imW / 2) / imW) * w / 2
      py = ((imH / 2 - fy) / imH) * h / 2
  in  Ray { x0 = Vec3 0 0 (-l), u = norm $ Vec3 px py l }


getPixel :: (Ray -> [VHit]) -> Camera -> Int -> Int -> PixelRGBF
getPixel collide cam x y = 
  let ray = computeInitialRay cam x y
      cs = collide ray
      frontFacing = filter (\c -> (u ray) .* (normal c) < 0) cs
      mc = sortOn (lensq . (x0 ray -) . point) frontFacing
  in  case mc of
        [] -> PixelRGBF 0 0 0
        hit@Hit {what = shader}:_ -> 
          let Vec3 r g b = shader (ShaderInput { ray = ray, hit = hit, lights = [], caster = \_ -> Vec3 0 0 0 })
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
          img = generateImage (getPixel collider camera) 800 600
      savePngImage (args !! 1) (ImageRGBF img)
