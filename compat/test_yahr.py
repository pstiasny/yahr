from os import getenv
from subprocess import call

from yahr import *

yahr_cmd = getenv('YAHR_CMD', 'cabal run -- ')


def test_parse():
    scene = Scene(
        WhittedIntegrator(3),
        BVH(16),
        Camera(
            imW=10,
            imH=10,
            focalLength=5,
            lookDir=Vec3(0, -0.1, -1),
            upDir=Vec3(0, 1, 0),
            position=Vec3(0, 0.2, 1)),
        [
            BlinnPhongMaterial(
                id="testmat",
                ambient=Vec3(0, 0, 0),
                diffuse=Vec3(0.8, 0.8, 0.8),
                specular=Vec3(0, 0, 0),
                shininess=1),
        ],
        [
            PointLight(Vec3(4, 10, 10))
        ],
        [
            TriangleMesh(
                [
                    Vec3(-1000, -2.2, -1000),
                    Vec3(1000, -2.2, -1000),
                    Vec3(1000, -2.2, 1000),
                    Vec3(-1000, -2.2, 1000),
                ],
                [
                    (0, 1, 2),
                    (0, 2, 3),
                ],
                "testmat")
        ])

    repr_ = scene.repr()
    with open('.testscene.yahr', 'w') as f:
        f.write(repr_)
    assert call(yahr_cmd + ' .testscene.yahr .testout.png', shell=True) == 0
