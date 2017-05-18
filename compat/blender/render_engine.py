from os import path
from subprocess import call

import bpy
from mathutils import Vector, Matrix
from .yahr import *


class YahrRenderEngine(bpy.types.RenderEngine):
    bl_idname = 'yahr_renderer'
    bl_label = 'Yahr Renderer'
    bl_use_preview = False

    _flip = Matrix.Scale(-1, 4, Vector((1, 0, 0)))

    def render(self, scene):
        scale = scene.render.resolution_percentage / 100.0
        self.size_x = int(scene.render.resolution_x * scale)
        self.size_y = int(scene.render.resolution_y * scale)

        result = self.begin_result(0, 0, self.size_x, self.size_y)
        layer = result.layers[0]

        scene_path = path.join(bpy.app.tempdir, 'blender_scene.yahr')
        image_path = path.join(bpy.app.tempdir, 'blender_scene.png')
        with open(scene_path, 'w') as f:
            f.write(self.make_scene(scene).repr())
        call([path.join(path.dirname(path.abspath(__file__)), 'yahr'),
              '--parallel-eval', scene_path, image_path, '+RTS', '-N2'])
        layer.load_from_file(image_path)

        self.end_result(result)

    def make_scene(self, scene):
        camera = self.make_camera(scene, scene.camera)
        objects = [
            self.make_mesh(scene, o)
            for o in scene.objects
            if o.type == 'MESH'
        ]
        lights = [
            self.make_light(scene, o)
            for o in scene.objects
            if o.type == 'LAMP'
        ]
        return Scene(
            WhittedIntegrator(2),
            BVH(16),
            camera,
            [
                BlinnPhongMaterial(
                    id="mat",
                    ambient=Vec3(0.9, 0.9, 0.9),
                    diffuse=Vec3(1, 1, 1),
                    specular=Vec3(0.3, 0.3, 0.3),
                    shininess=1),
            ],
            lights,
            objects)

    def make_mesh(self, scene, object):
        m = object.to_mesh(scene, True, 'RENDER')
        vertices = [
            self.blender_to_yahr_vec(self._flip * object.matrix_world * v.co)
            for v in m.vertices
        ]
        triangles = []
        flip = True
        for face in m.tessfaces:
            if len(face.vertices) == 3:
                triangles.append((
                    face.vertices[0], face.vertices[1],face.vertices[2]
                ))
            elif len(face.vertices) == 4:
                triangles.append((
                    face.vertices[0], face.vertices[1],face.vertices[2]
                ))
                triangles.append((
                    face.vertices[0], face.vertices[2],face.vertices[3]
                ))
            else:
                raise Exception('only triangles and quads are supported')
        return TriangleMesh(vertices, triangles, "mat")

    def make_camera(self, scene, camera):
        up = self._flip * camera.matrix_world * Vector((0.0, 1.0, 0.0, 0.0))
        fwd = self._flip * camera.matrix_world * Vector((0.0, 0.0, -1.0, 0.0))

        return Camera(
            imW=self.size_x,
            imH=self.size_y,
            focalLength=2,
            lookDir=self.blender_to_yahr_vec(fwd),
            upDir=self.blender_to_yahr_vec(up),
            position=self.blender_to_yahr_vec(self._flip * camera.location))

    def make_light(self, scene, object):
        lamp = object.data
        pos = self.blender_to_yahr_vec(self._flip * object.location)
        return PointLight(pos)
    
    def blender_to_yahr_vec(self, v):
        return Vec3(v[0], v[1], v[2])
