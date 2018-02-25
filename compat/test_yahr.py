from os import getenv, unlink, stat
from os.path import exists
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
            PointLight(Vec3(4, 10, 10), Vec3(1, 1, 1)),
        ],
        [
            SceneObject('testmesh', ['testmat'])
        ])

    with open('.testscene.yahr', 'w') as f:
        mesh_writer = MeshWriter(f)
        mesh_writer.begin('testmesh', 4, 2)

        mesh_writer.write_point(Vec3(-1000, -2.2, -1000))
        mesh_writer.write_point(Vec3(1000, -2.2, -1000))
        mesh_writer.write_point(Vec3(1000, -2.2, 1000))
        mesh_writer.write_point(Vec3(-1000, -2.2, 1000))

        mesh_writer.write_normal(Vec3(0, 1, 0))
        mesh_writer.write_normal(Vec3(0, 1, 0))
        mesh_writer.write_normal(Vec3(0, 1, 0))
        mesh_writer.write_normal(Vec3(0, 1, 0))

        mesh_writer.write_triangle(0, 1, 2, 0, False)
        mesh_writer.write_triangle(0, 2, 3, 0, True)

        mesh_writer.end()

        f.write(scene.repr())
        f.write('END\n')

    outpath = '.testout.png'
    if exists(outpath):
        unlink(outpath)

    assert call(yahr_cmd + ' .testscene.yahr ' + outpath, shell=True) == 0

    # check if output exists and is nonempty
    out_stat = stat(outpath)
    assert out_stat.st_size > 0
