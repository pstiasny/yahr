{-# LANGUAGE OverloadedStrings #-}

module Scene where

import Vectors
import qualified Cameras as C


data SceneObject =
     Sphere { position :: Vec3,
              radius :: Float,
              materialId :: String
            } 
   | Plane { position :: Vec3,
             planeNormal :: Vec3,
             materialId :: String
           } deriving (Read, Show)

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

