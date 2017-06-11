from collections import defaultdict
from os import path
from subprocess import call

import bpy
from mathutils import Vector, Matrix
from .yahr import *


class YahrRenderEngine(bpy.types.RenderEngine):
    bl_idname = 'yahr_renderer'
    bl_label = 'Yahr Renderer'
    bl_use_preview = False

    default_material = BlinnPhongMaterial(
        id='__default__',
        ambient=Vec3(0, 0, 0),
        diffuse=Vec3(1, 1, 1),
        specular=Vec3(0.3, 0.3, 0.3),
        shininess=1)

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

        if scene.yahr.parallelism_mode == 'EVAL':
            parallelism_mode_switch = '--parallel-eval'
        else:
            parallelism_mode_switch = '--parallel-disabled'

        threads = '-N{}'.format(scene.yahr.threads)

        call([path.join(path.dirname(path.abspath(__file__)), 'yahr'),
              parallelism_mode_switch, scene_path, image_path,
              '+RTS', threads])
        layer.load_from_file(image_path)

        self.end_result(result)

    def make_scene(self, scene):
        camera = self.make_camera(scene, scene.camera)
        materials = {self.default_material.id: self.default_material}
        objects = []
        for o in scene.objects:
            if o.type == 'MESH':
                objects.extend(self.make_meshes(scene, materials, o))
        lights = [
            self.make_light(scene, o)
            for o in scene.objects
            if o.type == 'LAMP'
        ]
        return Scene(
            WhittedIntegrator(2),
            BVH(16),
            camera,
            materials.values(),
            lights,
            objects)

    def make_meshes(self, scene, materials, object):
        m = object.to_mesh(scene, True, 'RENDER')

        # NOTE: To support blender's per-face materials, the mesh has to
        # be split into separate objects for each material.  Currently,
        # the whole triangle list is copied for each object.  This should
        # be optimized.
        vertices = [
            self.blender_to_yahr_vec(self._flip * object.matrix_world * v.co)
            for v in m.vertices
        ]
        normals = [
            self.blender_to_yahr_vec(
                self._flip * object.matrix_world *
                Vector((v.normal.x, v.normal.y, v.normal.z, 0)))
            for v in m.vertices
        ]
        triangles_by_material_index = (
            [[] for _ in m.materials] if m.materials else
            [[]]
        )
        smooth_by_material_index = (
            [[] for _ in m.materials] if m.materials else
            [[]]
        )
        for face in m.tessfaces:
            triangles = triangles_by_material_index[face.material_index]
            smooth = smooth_by_material_index[face.material_index]
            if len(face.vertices) == 3:
                triangles.append((
                    face.vertices[0], face.vertices[1], face.vertices[2]
                ))
                smooth.append(face.use_smooth)
            elif len(face.vertices) == 4:
                triangles.append((
                    face.vertices[0], face.vertices[1], face.vertices[2]
                ))
                triangles.append((
                    face.vertices[0], face.vertices[2], face.vertices[3]
                ))
                smooth.append(face.use_smooth)
                smooth.append(face.use_smooth)
            else:
                raise Exception('only triangles and quads are supported')

        if m.materials:
            meshes = []
            for bl_material, triangles, smooth in zip(
                    m.materials,
                    triangles_by_material_index,
                    smooth_by_material_index):
                try:
                    material = materials[bl_material.name]
                except KeyError:
                    material = self.make_material(scene, bl_material)
                    materials[bl_material.name] = material
                mesh = TriangleMesh(
                    vertices, normals, triangles, smooth, material.id)
                meshes.append(mesh)
            return meshes
        else:
            return [
                TriangleMesh(vertices, normals,
                             triangles_by_material_index[0],
                             smooth_by_material_index[0],
                             self.default_material.id)
            ]

    def make_material(self, scene, material):
        return BlinnPhongMaterial(
            id=material.name,
            ambient=Vec3(0.0, 0.0, 0.0),
            diffuse=self.blender_to_yahr_vec(
                material.diffuse_intensity * material.diffuse_color),
            specular=self.blender_to_yahr_vec(
                material.specular_intensity * material.specular_color),
            shininess=material.specular_hardness)

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
