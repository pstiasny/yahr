{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Scene where

import Control.Applicative ((<|>))
import Control.DeepSeq (NFData, rnf)
import Linear.V3 (V3 (V3))
import GHC.Word (Word8)
import Data.Maybe (fromMaybe)
import qualified Data.Map as M
import qualified Data.Vector as A
import qualified Data.Vector.Unboxed as U
import Data.Attoparsec.ByteString (Parser, take, inClass, string, satisfy, takeWhile, takeTill, many', choice)
import Data.Attoparsec.ByteString.Char8 ((<?>), char8, isEndOfLine, endOfLine, isDigit_w8, decimal, skipSpace, option, rational)
import Data.ByteString.Char8 (unpack)

import Vectors
import qualified Cameras as C
import qualified Culling
import qualified Integrators as I
import qualified Lights as L
import Shaders (Material)


data Command =
  MaterialCommand Material |
  MeshCommand Mesh |
  SceneCommand Scene |
  EndCommand


mesh :: Parser Command
mesh = do
  string "MESH " <?> "MESH"
  n <- decimal
  skipSpace
  m <- decimal
  skipSpace
  meshId <- fmap unpack $ takeTill isEndOfLine
  endOfLine

  string "POINTS" <?> "POINTS"
  endOfLine
  meshPoints <- U.replicateM n v3

  string "NORMALS" <?> "NORMALS"
  endOfLine
  meshNormals <- U.replicateM n v3

  string "TRIANGLES" <?> "TRIANGLES"
  endOfLine
  meshTriangles <- U.replicateM m $ do
    i1 <- decimal
    skipSpace
    i2 <- decimal
    skipSpace
    i3 <- decimal
    skipSpace
    matIdx <- decimal
    skipSpace
    smooth <- (string "True" >> return True) <|> (string "False" >> return False)
    endOfLine
    return (i1, i2, i3, matIdx, smooth)

  string "END MESH" <?> "END MESH"
  endOfLine
  return $ MeshCommand $ Mesh { meshId, meshPoints, meshTriangles, meshNormals }

  where v3 = do
          x <- rational
          skipSpace
          y <- rational
          skipSpace
          z <- rational
          endOfLine
          return (V3 x y z)

scene :: Parser Command
scene = do
  string "SCENE " <?> "SCENE"
  scene <- fmap (read . unpack) $ takeTill isEndOfLine
  endOfLine
  return (SceneCommand scene)

end :: Parser Command
end = do
  string "END" <?> "END"
  endOfLine
  return EndCommand

input :: Parser Command
input = choice
    [ mesh <?> "mesh"
    , scene <?> "scene"
    , end <?> "end"
    ] <?> "command"


data Mesh = Mesh
     { meshId :: String
     , meshPoints :: U.Vector Vec3
     , meshNormals :: U.Vector Vec3
     , meshTriangles :: U.Vector (Int, Int, Int, Int, Bool)
     }
   deriving (Read, Show)


instance NFData Mesh where
  rnf (Mesh { meshId, meshPoints, meshNormals, meshTriangles }) =
    rnf meshId `seq` rnf meshPoints `seq` rnf meshNormals
    `seq` rnf meshTriangles 


data Triangle = Triangle {
       tMesh :: Mesh,
       tMeshIdx :: Int,
       tMaterial :: Material
     }

p0 :: Triangle -> Vec3
p0 (Triangle {tMesh, tMeshIdx}) = (meshPoints tMesh U.! i0)
  where (i0, _, _, _, _) = meshTriangles tMesh U.! tMeshIdx

p1 :: Triangle -> Vec3
p1 (Triangle {tMesh, tMeshIdx}) = (meshPoints tMesh U.! i1)
  where (_, i1, _, _, _) = meshTriangles tMesh U.! tMeshIdx

p2 :: Triangle -> Vec3
p2 (Triangle {tMesh, tMeshIdx}) = (meshPoints tMesh U.! i2)
  where (_, _, i2, _, _) = meshTriangles tMesh U.! tMeshIdx

n0 :: Triangle -> Vec3
n0 (Triangle {tMesh, tMeshIdx}) = (meshNormals tMesh U.! i0)
  where (i0, _, _, _, _) = meshTriangles tMesh U.! tMeshIdx

n1 :: Triangle -> Vec3
n1 (Triangle {tMesh, tMeshIdx}) = (meshNormals tMesh U.! i1)
  where (_, i1, _, _, _) = meshTriangles tMesh U.! tMeshIdx

n2 :: Triangle -> Vec3
n2 (Triangle {tMesh, tMeshIdx}) = (meshNormals tMesh U.! i2)
  where (_, _, i2, _, _) = meshTriangles tMesh U.! tMeshIdx

smooth :: Triangle -> Bool
smooth  (Triangle {tMesh, tMeshIdx}) = s
  where (_, _, _, _, s) = meshTriangles tMesh U.! tMeshIdx

data SceneObject = SceneObject
    { sceneObjectMeshId :: String
    , sceneObjectMaterialIds :: [String]
    }
   deriving (Read, Show)
    

data Scene = Scene { integrator :: I.IntegratorSpec,
                     cullingMode :: Culling.CullingMode,
                     camera :: C.Camera,
                     materials :: [ Material ],
                     lights :: [ L.Light ],
                     objects :: [ SceneObject ]
                   } deriving (Read, Show)


expand :: M.Map String Material -> M.Map String Mesh -> SceneObject -> [Triangle]
expand materials meshes (SceneObject { sceneObjectMeshId, sceneObjectMaterialIds }) =
  let mesh@Mesh { meshId, meshPoints, meshNormals, meshTriangles } =
        meshes M.! sceneObjectMeshId
      objectMaterials = A.fromList ((materials M.!) <$> sceneObjectMaterialIds)
      triangleOfPoints idx (_, _, _, mi, smooth) =
        Triangle {tMesh = mesh, tMeshIdx = idx, tMaterial = material}
        where material = objectMaterials A.! mi
  in  zipWith triangleOfPoints [0..] (U.toList meshTriangles)
