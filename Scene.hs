{-# LANGUAGE NamedFieldPuns #-}

module Scene where

import Data.Maybe (fromMaybe)
import qualified Data.Vector as A

import Vectors
import qualified Cameras as C
import qualified Culling
import qualified Integrators as I


data SceneObject =
     Sphere {
       position :: Vec3,
       radius :: Float,
       materialId :: String
     } 
   | Triangle {
       p0 :: Vec3,
       p1 :: Vec3,
       p2 :: Vec3,
       n0 :: Vec3,
       n1 :: Vec3,
       n2 :: Vec3,
       materialId :: String
     }
   | TriangleMesh {
       triangleMeshPoints :: [Vec3],
       triangleMeshNormals :: Maybe [Vec3],
       triangleMeshTriangles :: [(Int, Int, Int)],
       triangleMeshSmooth :: Maybe [Bool],
       materialId :: String
     }
   | Subsampled {
       subsampleSize :: Float,
       subsampledObjects :: [SceneObject]
     }

   deriving (Read, Show)
    

data Material = BlinnPhongMaterial { id :: String,
                                     ambient :: Spectrum,
                                     diffuse :: Spectrum,
                                     specular :: Spectrum,
                                     shininess :: Float
                                   } deriving (Read, Show)

data SceneLight = PointLight Vec3 deriving (Read, Show)

data Scene = Scene { integrator :: I.IntegratorSpec,
                     cullingMode :: Culling.CullingMode,
                     camera :: C.Camera,
                     materials :: [ Material ],
                     lights :: [ SceneLight ],
                     objects :: [ SceneObject ]
                   } deriving (Read, Show)


expand :: SceneObject -> [SceneObject]
expand (TriangleMesh {triangleMeshPoints, triangleMeshTriangles,
                      triangleMeshNormals, triangleMeshSmooth, materialId}) =
  let points = A.fromList triangleMeshPoints
      mNormals = fmap A.fromList triangleMeshNormals
      smooth = fromMaybe (repeat False) triangleMeshSmooth
      triangleOfPoints (i0, i1, i2) smooth = case mNormals of
        Just normals -> Triangle p0 p1 p2 n0 n1 n2 materialId
          where (n0, n1, n2) =
                  if smooth
                    then (normals A.! i0, normals A.! i1, normals A.! i2)
                    else (n, n, n)
        Nothing -> Triangle p0 p1 p2 n n n materialId
        where 
          p0 = (points A.! i0)
          p1 = (points A.! i1)
          p2 = (points A.! i2)
          n = norm $ (p2 - p0) `cross` (p1 - p0)
  in  zipWith triangleOfPoints triangleMeshTriangles smooth
expand (Subsampled { subsampledObjects, subsampleSize }) =
  let ofHundred = ceiling (subsampleSize * 100)
      pick [] = []
      pick xs = picked ++ pick (drop (100 - ofHundred) rest)
                  where (picked, rest) = splitAt ofHundred xs
  in  pick (subsampledObjects >>= expand)
expand x = [x]
