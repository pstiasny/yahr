{-# LANGUAGE NamedFieldPuns #-}
import Control.Monad (forM_)
import Data.Maybe (isJust, isNothing)
import Data.List (sort)
import qualified Data.Set as Set

import Test.Hspec

import Vectors
import Rays
import Cameras
import qualified Shapes
import qualified Scene
import qualified AABBs
import qualified Culling
import qualified BSDF
import qualified Shaders
import qualified Integrators
import qualified Sampling
import DifferentialGeometry

near :: Vec3 -> Vec3 -> Bool
near a b = len (a - b) < 0.0001

passesThrough :: Vec3 -> Ray -> Bool
passesThrough x (Ray {x0, u}) =
  let dimSel = getDimension $ maxDimension (abs u)
      t = (dimSel x - dimSel x0) / dimSel u
      tu = t @* u
  in  len (x - x0 - tu) < 0.0001

main :: IO ()
main = hspec $ do
  describe "Vector" $ do
    context "cross product" $ do
      it "forward x up yields left" $ do
        let product = (Vec3 0 0 1) `cross` (Vec3 0 1 0)
        product `shouldSatisfy` near (Vec3 (-1) 0 0)

      it "up x forward yields right" $ do
        let product = (Vec3 0 1 0) `cross` (Vec3 0 0 1)
        product `shouldSatisfy` near (Vec3 1 0 0)

      it "left x forward yields up" $ do
        let product = (Vec3 (-1) 0 0) `cross` (Vec3 0 0 1)
        product `shouldSatisfy` near (Vec3 0 1 0)

  describe "Ray" $ do
    it "should hit nearest of available collision candidates" $ do
      let n = Vec3 0 0 (-1)
          hit1 = Hit 1 undefined 1
          hit2 = Hit 2 undefined 2
          colliders = [
            const Nothing,
            const (Just hit2),
            const (Just hit1) ]
          ray = Ray { x0 = vof 0, u = vof 1, tMax = 10 }
          (Just hit) = collideAll colliders ray
      what hit `shouldBe` 1

  describe "Camera" $ do
    let origin = Vec3 10 (-80) 124
        lookFromOrigin up dir = Camera { imW = 200, imH = 200,
                                         focalLength = 1,
                                         lookDir = dir,
                                         upDir = up,
                                         position = origin }

    context "looking forward in world space" $ do
      let c = lookFromOrigin (Vec3 0 1 0) (Vec3 0 0 1)

      it "casts rays through the middle of the imaging frame" $ do
        let r@Ray { x0, u } = computeInitialRay c 100 100
        x0 `shouldSatisfy` near origin
        u `shouldSatisfy` near (Vec3 0 0 1)
        r `shouldSatisfy` passesThrough (origin + Vec3 0 0 1)

      it "casts rays through the top left corner of the imaging frame" $ do
        let r@Ray { x0, u } = computeInitialRay c 0 0
        x0 `shouldSatisfy` near origin
        r `shouldSatisfy` passesThrough (origin + Vec3 (-0.5) (0.5) 1)

    context "looking down in world space" $ do
      let c = lookFromOrigin (Vec3 0 0 1) (Vec3 0 (-1) 0)

      it "casts rays through the middle of the imaging frame" $ do
        let Ray { x0, u } = computeInitialRay c 100 100
        x0 `shouldSatisfy` near origin
        u `shouldSatisfy` near (Vec3 0 (-1) 0)

      it "casts rays through the top left corner of the imaging frame" $ do
        let r@Ray { x0, u } = computeInitialRay c 0 0
        x0 `shouldSatisfy` near origin
        r `shouldSatisfy` passesThrough (origin + Vec3 (-0.5) (-1) 0.5)

    context "looking right in world space" $ do
      let c = lookFromOrigin (Vec3 0 1 0) (Vec3 1 0 0)

      it "casts rays through the middle of the imaging frame" $ do
        let Ray { x0, u } = computeInitialRay c 100 100
        x0 `shouldSatisfy` near origin
        u `shouldSatisfy` near (Vec3 1 0 0)

      it "casts rays through the top left corner of the imaging frame" $ do
        let r@Ray { x0, u } = computeInitialRay c 0 0
        x0 `shouldSatisfy` near origin
        r `shouldSatisfy` passesThrough (origin + Vec3 1 0.5 0.5)

  describe "Shapes" $ do
    describe "Sphere" $ do
      it "is fully bound by an AABB" $ do
        let (AABBs.BoundingBox bMin bMax) = Shapes.boundSphere 5 (Vec3 10 10 15)
        bMin `shouldSatisfy` near (Vec3 5 5 10)
        bMax `shouldSatisfy` near (Vec3 15 15 20)

    describe "Triangle" $ do
      let n = Vec3 0 0 (-1)
          tc = Shapes.collideTriangle (1::Int) (Vec3 0 0 5) (Vec3 2 0 5) (Vec3 1 2 5) n n n
          tbb = Shapes.boundTriangle (Vec3 0 0 5) (Vec3 2 0 5) (Vec3 1 2 5)

      it "reflects rays intersecting with it" $ do
        let testPoint x y = do
              let mhit = tc (Ray { x0 = Vec3 x y 0, u = Vec3 0 0 1, tMax = 1e6 })
              mhit `shouldSatisfy` isJust
              let Just hit = mhit
              point hit `shouldSatisfy` near (Vec3 x y 5)
              normal hit `shouldSatisfy` near (Vec3 0 0 (-1))
              what hit `shouldBe` (1 :: Int)
        testPoint 1 1
        testPoint 0.1 0.1
        testPoint 0.9 0.1

      it "does not reflect rays not intersecting with it" $ do
        let testPoint x y = do
              let mhit = tc (Ray { x0 = Vec3 x y 0, u = Vec3 0 0 1, tMax = 1e6 })
              mhit `shouldSatisfy` isNothing
        testPoint 0 1
        testPoint (-0.1) 1
        testPoint 2 1

      it "is fully bound by an AABB" $ do
        let (AABBs.BoundingBox bMin bMax) = tbb
        bMin `shouldSatisfy` near (Vec3 0 0 5)
        bMax `shouldSatisfy` near (Vec3 2 2 5)


  describe "Scene" $ do
    describe "TriangleMesh" $ do
      it "gets expanded to a list of triangles" $ do
        let tm = Scene.TriangleMesh {
                   Scene.triangleMeshPoints = [
                     Vec3 0 0 0, Vec3 0 0 1,
                     Vec3 0 1 0, Vec3 0 1 1
                     ],
                   Scene.triangleMeshNormals = Nothing,
                   Scene.triangleMeshTriangles = [
                     (0, 1, 2), (1, 2, 3)
                     ],
                   Scene.triangleMeshSmooth = Nothing,
                   Scene.materialId = "fooMaterial"
                   }
            [expandedT1, expandedT2] = Scene.expand tm

        Scene.p0 expandedT1 `shouldSatisfy` near (Vec3 0 0 0)
        Scene.p1 expandedT1 `shouldSatisfy` near (Vec3 0 0 1)
        Scene.p2 expandedT1 `shouldSatisfy` near (Vec3 0 1 0)
        Scene.p0 expandedT2 `shouldSatisfy` near (Vec3 0 0 1)
        Scene.p1 expandedT2 `shouldSatisfy` near (Vec3 0 1 0)
        Scene.p2 expandedT2 `shouldSatisfy` near (Vec3 0 1 1)

        Scene.materialId expandedT1 `shouldBe` "fooMaterial"
        Scene.materialId expandedT2 `shouldBe` "fooMaterial"


  describe "AABB" $ do
    it "should bound two points" $ do
      let (AABBs.BoundingBox pMin pMax) = AABBs.fromPoints (Vec3 1 20 5)
                                                           (Vec3 14 4 3)
      pMin `shouldSatisfy` near (Vec3 1 4 3)
      pMax `shouldSatisfy` near (Vec3 14 20 5)

    it "should bound multiple points" $ do
      let bb = AABBs.BoundingBox (Vec3 1 4 3) (Vec3 14 20 5)
          (AABBs.BoundingBox pMin pMax) = AABBs.includePoint bb (Vec3 0 22 3)
      pMin `shouldSatisfy` near (Vec3 0 4 3)
      pMax `shouldSatisfy` near (Vec3 14 22 5)

    it "should bound two existing AABBs" $ do
      let bb1 = AABBs.BoundingBox (Vec3 1 4 3) (Vec3 14 20 5)
          bb2 = AABBs.BoundingBox (Vec3 0 4 2) (Vec3 15 20 1)
          (AABBs.BoundingBox pMin pMax) = AABBs.join bb1 bb2
      pMin `shouldSatisfy` near (Vec3 0 4 2)
      pMax `shouldSatisfy` near (Vec3 15 20 5)

    let bb = AABBs.BoundingBox (Vec3 10 20 30) (Vec3 20 30 40)
        collideAny = const $ Just (Hit 0 undefined True)
        bbc = AABBs.wrapCollider collideAny bb

    it "should not be hit by rays missing it" $ do
      bbc (Ray { x0 = Vec3 9 29 35, u = norm $ Vec3 1 1.1 0, tMax = 1e6 })
        `shouldSatisfy` isNothing
      bbc (Ray { x0 = Vec3 21 29 35, u = norm $ Vec3 (-1) 1.1 0, tMax = 1e6 })
        `shouldSatisfy` isNothing
      bbc (Ray { x0 = Vec3 15 19 39, u = norm $ Vec3 0 1 1.1, tMax = 1e6 })
        `shouldSatisfy` isNothing

    it "should propagate rays intersecting it" $ do
      bbc (Ray { x0 = Vec3 9 29 35, u = norm $ Vec3 1 0.9 0, tMax = 1e6 })
        `shouldSatisfy` isJust
      bbc (Ray { x0 = Vec3 21 29 35, u = norm $ Vec3 (-1) 0.9 0, tMax = 1e6 })
        `shouldSatisfy` isJust
      bbc (Ray { x0 = Vec3 15 19 39, u = norm $ Vec3 0 1 0.9, tMax = 1e6 })
        `shouldSatisfy` isJust

    it "should have the centroid in its middle" $ do
      let bb = AABBs.BoundingBox (vof (-10)) (vof 10)
      AABBs.centroid bb `shouldSatisfy` near (vof 0)


  describe "Culling" $ do
    describe "BVH" $ do
      let grid = [(i, j, k) | i <- [1..10], j <- [1..10], k <- [1,2]]
          sphereCenters = [Vec3 (i*10) (j*10) (k*10) | (i,j,k) <- grid]

      context "using SAH splitting" $ do
        let cm = Culling.BVH 16 Culling.SurfaceAreaHeuristic
            root = Culling.cull cm $ [(Shapes.boundSphere 1 c, Shapes.collideSphere i 1 c)
                                     | (i, c) <- zip grid sphereCenters]

        it "should propagate rays to correct shapes" $ do
          let testRay x y i = do
                let mhit = root (Ray { x0 = Vec3 x y 0, u = Vec3 0 0 1, tMax = 1e6 })
                mhit `shouldSatisfy` isJust
                let Just hit = mhit
                what hit `shouldBe` i
          testRay 10 10 (1, 1, 1)
          testRay 30 30 (3, 3, 1)
          testRay 10 100 (1, 10, 1)
          testRay 100 10 (10, 1, 1)
          testRay 50 50 (5, 5, 1)

          testRay 10.49 10.49 (1, 1, 1)

      context "using midpoint splitting" $ do
        let cm = Culling.BVH 16 Culling.Midpoint
            root = Culling.cull cm $ [(Shapes.boundSphere 1 c, Shapes.collideSphere i 1 c)
                                     | (i, c) <- zip grid sphereCenters]

        it "should propagate rays to correct shapes" $ do
          let testRay x y i = do
                let mhit = root (Ray { x0 = Vec3 x y 0, u = Vec3 0 0 1, tMax = 1e6 })
                mhit `shouldSatisfy` isJust
                let Just hit = mhit
                what hit `shouldBe` i
          testRay 10 10 (1, 1, 1)
          testRay 30 30 (3, 3, 1)
          testRay 10 100 (1, 10, 1)
          testRay 100 10 (10, 1, 1)
          testRay 50 50 (5, 5, 1)

          testRay 10.49 10.49 (1, 1, 1)


  describe "Integrator" $ do
    let bsdfTriangle bsdf = Shapes.collideTriangle mat p0 p1 p2 n n n
          where mat = Shaders.Material (\_ _ -> bsdf)
                p0 = Vec3 (-10) (-10) 0
                p1 = Vec3 10 (-10) 0
                p2 = Vec3 0 10 0
                n = Vec3 0 0 1
        integrator = Integrators.WhittedIntegrator { Integrators.recursionDepth = 1 }
        vec angle = Vec3 (cos angle) 0 (sin angle)
        pt angle = negate $ vec angle
        eye angle = Ray { x0 = pt angle, u = vec angle, tMax = 1e6 }

        bsdf = BSDF.Blinn 10
        collider = bsdfTriangle bsdf

    it "should not reflect on the rear side" $ do
      let ray = Ray { x0 = Vec3 0 0 (-1), u = Vec3 0 0 1, tMax = 1e6 }
          light = Vec3 (cos (pi/4)) 0 (sin (pi/4))
          rad = Integrators.radiance integrator [light] collider ray
      rad `shouldSatisfy` near (vof 0)

      let ray = eye (1.5*pi)
          light = pt (0.25*pi)
          rad = Integrators.radiance integrator [light] collider ray
      rad `shouldSatisfy` near (vof 0)

    it "should reflect the most when incident angle equals reflected angle" $ do
      let ray = eye (0.25*pi)
          lights = [pt angle | angle <- [0.25*pi, 0.50*pi, 0.75*pi]]
          rads = [lensq $ Integrators.radiance integrator [light] collider ray
                 | light <- lights]
      rads `shouldBe` sort rads


  describe "BSDF" $ do
    let vec angle = Vec3 (cos angle) 0 (sin angle)
        dg = DifferentialGeometry {
               dgPoint = Vec3 0 0 0,
               dgNormal = Vec3 0 0 1,
               dgDPDU = Vec3 1 0 0,
               dgDPDV = Vec3 0 1 0
             }

    describe "Blinn" $ do
      let bsdf = BSDF.Blinn 10

      it "should not reflect on the rear side" $ do
        let eye = vec (7*pi/4)
            light = vec (5*pi/4)
            f = BSDF.at bsdf dg light eye
        f `shouldSatisfy` near (vof 0)

      it "should not transmit" $ do
        let eye = vec (0.25 * pi)
            light = vec (1.25 * pi)
        forM_ [1.25 * pi, 1.5 * pi, 1.75 * pi] $ \lightAngle ->
          BSDF.at bsdf dg (vec lightAngle) eye `shouldSatisfy` near (vof 0)

      it "should reflect the most when incident angle equals reflected angle" $ do
        let eye = vec (0.25 * pi)
            lights = [vec angle | angle <- [0.25 * pi, 0.50 * pi, 0.75 * pi, 0.9*pi]]
            fs = [lensq $ BSDF.at bsdf dg light eye | light <- lights]
        fs `shouldBe` sort fs

  describe "Sampling" $ do
    describe "squareBatches" $ do
      let dimensions = [(40, 40, 5), (40, 40, 16), (1, 1, 1), (20, 1, 3), (1, 20, 3)]

      it "cover the whole image" $ forM_ dimensions $ \(w, h, n) -> do
        let batches = Sampling.squareBatches w h n
            s = Set.fromList (concat batches)
        foldl (flip Set.delete) s [(u, v) | u <- [0..w-1], v <- [0..h-1]]
          `shouldSatisfy` Set.null

      it "do not overlap" $ forM_ dimensions $ \(w, h, n) -> do
        let samples = concat $ Sampling.squareBatches w h n
            hasOverlapping = snd $ foldl
              (\(s, b) x -> (Set.insert x s, Set.member x s || b))
              (Set.empty, False)
              samples
        hasOverlapping `shouldBe` False
