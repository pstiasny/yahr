{-# LANGUAGE NamedFieldPuns #-}

module Scene where

import qualified Data.Vector as A

import Vectors
import qualified Cameras as C
import qualified Culling


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
       materialId :: String
     }
   | TriangleMesh {
       triangleMeshPoints :: [Vec3],
       triangleMeshTriangles :: [(Int, Int, Int)],
       materialId :: String
     }
   | Subsampled {
       subsampleSize :: Float,
       subsampledObjects :: [SceneObject]
     }

   deriving (Read, Show)
    

data Material = BlinnPhongMaterial { id :: String,
                                     ambient :: Color,
                                     diffuse :: Color,
                                     specular :: Color,
                                     shininess :: Float
                                   } deriving (Read, Show)

data SceneLight = PointLight Vec3 deriving (Read, Show)

data Scene = Scene { cullingMode :: Culling.CullingMode,
                     camera :: C.Camera,
                     materials :: [ Material ],
                     lights :: [ SceneLight ],
                     objects :: [ SceneObject ]
                   } deriving (Read, Show)


expand :: SceneObject -> [SceneObject]
expand (TriangleMesh {triangleMeshPoints, triangleMeshTriangles, materialId}) =
  let points = A.fromList triangleMeshPoints
      triangleOfPoints (i0, i1, i2) = Triangle (points A.! i0) (points A.! i1)
                                               (points A.! i2) materialId
  in  map triangleOfPoints triangleMeshTriangles
expand (Subsampled { subsampledObjects, subsampleSize }) =
  let ofHundred = ceiling (subsampleSize * 100)
      pick [] = []
      pick xs = picked ++ pick (drop (100 - ofHundred) rest)
                  where (picked, rest) = splitAt ofHundred xs
  in  pick (subsampledObjects >>= expand)
expand x = [x]
