from collections import defaultdict
from os import path
from time import time
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
        t0 = time()

        scale = scene.render.resolution_percentage / 100.0
        self.size_x = int(scene.render.resolution_x * scale)
        self.size_y = int(scene.render.resolution_y * scale)

        result = self.begin_result(0, 0, self.size_x, self.size_y)
        layer = result.layers[0]

        scene_path = path.join(bpy.app.tempdir, 'blender_scene.yahr')
        image_path = path.join(bpy.app.tempdir, 'blender_scene.png')
        with open(scene_path, 'w') as f:
            self.dump_scene(f, scene)
        tdump = time()
        print('time to dump scene', tdump - t0)

        threads = '-N{}'.format(scene.yahr.threads)

        call([path.join(path.dirname(path.abspath(__file__)), 'yahr'),
              scene_path, image_path,
              '-p', scene.yahr.parallelism_mode.lower(),
              '+RTS', threads])
        layer.load_from_file(image_path)

        self.end_result(result)
        print('time to render', time() - tdump)

    def dump_scene(self, file_, scene):
        materials = {self.default_material.id: self.default_material}
        objects = []
        for o in scene.objects:
            if o.type == 'MESH':
                self.dump_mesh(file_, scene, materials, objects, o)

        camera = self.make_camera(scene, scene.camera)
        lights = [
            self.make_light(scene, o)
            for o in scene.objects
            if o.type == 'LAMP'
        ]
        scene_ = Scene(
            WhittedIntegrator(2),
            BVH(16),
            camera,
            materials.values(),
            lights,
            objects)
        file_.write(scene_.repr())

        file_.write('END\n')

    def dump_mesh(self, file_, scene, materials, objects, object):
        print('dumping object', object.name)
        writer = MeshWriter(file_)

        mx = self._flip * object.matrix_world

        m = object.to_mesh(scene, True, 'RENDER')

        for bl_material in m.materials:
            try:
                material = materials[bl_material.name]
            except KeyError:
                material = self.make_material(scene, bl_material)
                materials[bl_material.name] = material

        if m.materials:
            objects.append(
                SceneObject(object.name, [mat.name for mat in m.materials]))
        else:
            objects.append(
                SceneObject(object.name, [self.default_material.id]))

        triangle_count = 0
        for face in m.tessfaces:
            if len(face.vertices) == 3:
                triangle_count += 1
            elif len(face.vertices) == 4:
                triangle_count += 2
            else:
                raise Exception('only triangles and quads are supported')

        writer.begin(object.name, len(m.vertices), triangle_count)

        for v in m.vertices:
            vec3 = self.blender_to_yahr_vec(mx * v.co)
            writer.write_point(vec3)

        for v in m.vertices:
            vec3 = self.blender_to_yahr_vec(
                mx * Vector((v.normal.x, v.normal.y, v.normal.z, 0)))
            writer.write_normal(vec3)

        for face in m.tessfaces:
            if len(face.vertices) == 3:
                writer.write_triangle(
                    face.vertices[0], face.vertices[1], face.vertices[2],
                    face.material_index, face.use_smooth)
            elif len(face.vertices) == 4:
                writer.write_triangle(
                    face.vertices[0], face.vertices[1], face.vertices[2],
                    face.material_index, face.use_smooth)
                writer.write_triangle(
                    face.vertices[0], face.vertices[2], face.vertices[3],
                    face.material_index, face.use_smooth)
            else:
                raise Exception('only triangles and quads are supported')

        writer.end()
        bpy.data.meshes.remove(m)

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
        if lamp.type != 'POINT':
            print('warning: unsupported light type:', lamp.type)
        pos = self.blender_to_yahr_vec(self._flip * object.location)
        spectrum = self.blender_to_yahr_vec(lamp.energy * lamp.color)
        return PointLight(pos, spectrum)
    
    def blender_to_yahr_vec(self, v):
        return Vec3(v[0], v[1], v[2])
