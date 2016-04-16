{-# LANGUAGE NamedFieldPuns #-}

module Scene where

import qualified Data.Vector as A

import Vectors
import qualified Cameras as C


data SceneObject =
     Sphere {
       position :: Vec3,
       radius :: Float,
       materialId :: String
     } 
   | Plane {
       position :: Vec3,
       planeNormal :: Vec3,
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

   deriving (Read, Show)
    

data Material = BlinnPhongMaterial { id :: String,
                                     ambient :: Color,
                                     diffuse :: Color,
                                     specular :: Color,
                                     shininess :: Float
                                   } deriving (Read, Show)

data SceneLight = PointLight Vec3 deriving (Read, Show)

data Scene = Scene { camera :: C.Camera,
                     materials :: [ Material ],
                     lights :: [ SceneLight ],
                     objects :: [ SceneObject ]
                   } deriving (Read, Show)

expand :: SceneObject -> [SceneObject]
expand (TriangleMesh {triangleMeshPoints, triangleMeshTriangles, materialId}) =
  let points = A.fromList triangleMeshPoints
      triangleOfPoints (i0, i1, i2) = Triangle (points A.! i0) (points A.! i1) (points A.! i2) materialId
  in  map triangleOfPoints triangleMeshTriangles
expand x = [x]
