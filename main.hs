{-# LANGUAGE NamedFieldPuns #-}

import Data.List (sortOn)
import Data.Maybe (isJust, fromJust)
import Data.Map (Map, fromList, (!))
import System.Environment
import Codec.Picture

import Data.Vectors
import qualified Data.Scene as Scene
import Data.Scene as Scene hiding (lights)

data Hit a = Hit { point :: Vec3, normal :: Vec3, what :: a }

type Light = Vec3
type Normal = Vec3
data ShaderInput = ShaderInput { ray :: Ray,
                                 hit :: Hit Shader,
                                 lights :: [Light],
                                 cast :: Normal -> Color
                               }
type Shader = ShaderInput -> Color
type VHit = Hit Shader
type Collider a = Ray -> Maybe (Hit a)

-- Ray described by l(t) = x0 + t * u
data Ray = Ray { x0 :: Vec3, u :: Vec3 }

data Camera = Camera { imW :: Float, imH :: Float,
                       w :: Float, h :: Float, l :: Float
                     } deriving (Show)  


headMay [] = Nothing
headMay (h:_) = Just h


blinnPhong :: Vec3 -> Vec3 -> Vec3 -> Float -> [Vec3] -> Shader
blinnPhong ambientColor diffuseColor specularColor shininess
           lights ShaderInput { cast, ray, hit } =
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

collideSphere :: a -> Float -> Vec3 -> Collider a
collideSphere shader r s (Ray {x0, u}) =
  let d = x0 - s
      a = u .* u
      b = 2 * d .* u
      c = d .* d - r * r
      delta = b * b - 4 * a * c
      ts = if delta < 0 then [] else [ (- b - sqrt delta) / (2 * a)
                                     , (- b + sqrt delta) / (2 * a) ]
      xs = [ x0 + t @* u | t <- ts, t > 0 ]
  in  headMay [Hit { point = x, normal = norm $ x - s, what = shader }
              | x <- xs]


collidePlane :: a -> Vec3 -> Vec3 -> Collider a
collidePlane shader origin normal (Ray {x0, u}) =
  let t = (normal .* (origin - x0)) / (normal .* u)
  in  if   t > 0
      then Just (Hit { point = x0 + t @* u, normal = normal, what = shader })
      else Nothing


collideAll :: [Collider a] -> Collider a
collideAll colliders ray =
  headMay $
    sortOn (lensq . (x0 ray -) . point) $
    map fromJust $
    filter isJust $
    map ($ ray) colliders


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
        blinnPhong ambient diffuse specular shininess lights
    lights = [v | PointLight v <- Scene.lights s]


computeInitialRay :: Camera -> Int -> Int -> Ray
computeInitialRay (Camera {imW, imH, w, h, l}) x y =
  let fx = fromIntegral x
      fy = fromIntegral y
      px = ((fx - imW / 2) / imW) * w / 2
      py = ((imH / 2 - fy) / imH) * h / 2
  in  Ray { x0 = Vec3 0 0 (-l), u = norm $ Vec3 px py l }


vcast :: Int -> Collider Shader -> Ray -> Color
vcast 0 _ _ = (Vec3 0 0 0)
vcast maxDepth collide ray =
  case collide ray of
    Nothing -> Vec3 0 0 0
    Just hit@Hit {point, what = shader} ->
      shader (ShaderInput { ray = ray,
                            hit = hit,
                            lights = [],
                            cast = castNext })
      where castNext n = vcast (maxDepth - 1) collide Ray { x0 = point + 0.01 @* n, u = n }

    
getPixel :: Collider Shader -> Camera -> Int -> Int -> PixelRGBF
getPixel collide cam x y = 
  let ray = computeInitialRay cam x y
      Vec3 r g b = vcast 5 collide ray
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
