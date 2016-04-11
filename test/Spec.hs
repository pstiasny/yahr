{-# LANGUAGE NamedFieldPuns #-}
import Test.Hspec

import Vectors
import Rays
import Cameras

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
