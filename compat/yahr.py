class Scene(object):
    def __init__(self, integrator, cullingMode, camera, materials, lights, objects):
        self.integrator = integrator
        self.cullingMode = cullingMode
        self.camera = camera
        self.materials = materials
        self.lights = lights
        self.objects = objects

    def repr(self):
        return (
            'SCENE Scene { integrator = %s, cullingMode = %s, camera = %s, '
            'materials = [ %s ], lights = [ %s ], objects = [ %s ] }\n'
        ) % (
            self.integrator.repr(),
            self.cullingMode.repr(),
            self.camera.repr(),
            ','.join(m.repr() for m in self.materials),
            ','.join(l.repr() for l in self.lights),
            ','.join(o.repr() for o in self.objects),
        )


class WhittedIntegrator(object):
    def __init__(self, recursionDepth):
        self.recursionDepth = recursionDepth

    def repr(self):
        return 'WhittedIntegrator { recursionDepth = %d }' % self.recursionDepth


class BVH(object):
    def __init__(self, maxDepth, splitMode='Midpoint'):
        self.maxDepth = maxDepth
        self.splitMode = splitMode

    def repr(self):
        return '(BVH %d %s)' % (self.maxDepth, self.splitMode)


class Camera(object):
    def __init__(self, imW, imH, focalLength, lookDir, upDir, position):
        self.imW = imW
        self.imH = imH
        self.focalLength = focalLength
        self.lookDir = lookDir
        self.upDir = upDir
        self.position = position

    def repr(self):
        return (
            'Camera { imW = %d, imH = %d, focalLength = %d, lookDir = %s, '
            'upDir = %s, position = %s }'
        ) % (
            self.imW, self.imH, self.focalLength, self.lookDir.repr(),
            self.upDir.repr(), self.position.repr(),
        )


class BlinnPhongMaterial(object):
    def __init__(self, id, ambient, diffuse, specular, shininess):
        self.id = id
        self.ambient = ambient
        self.diffuse = diffuse
        self.specular = specular
        self.shininess = shininess

    def repr(self):
        return (
            'BlinnPhongMaterial { materialId = "%s", ambient = %s, diffuse = %s, '
            'specular = %s, shininess = %f }'
        ) % (
            repr_string(self.id), self.ambient.repr(), self.diffuse.repr(),
            self.specular.repr(), self.shininess,
        )


class PointLight(object):
    def __init__(self, position, spectrum):
        self.position = position
        self.spectrum = spectrum

    def repr(self):
        return '(PointLight %s %s)' % (
            self.position.repr(),
            self.spectrum.repr(),
        )


class SceneObject(object):
    def __init__(self, sceneObjectMeshId, sceneObjectMaterialIds):
        self.sceneObjectMeshId = sceneObjectMeshId
        self.sceneObjectMaterialIds = sceneObjectMaterialIds

    def repr(self):
        return (
            'SceneObject {'
            'sceneObjectMeshId = "%s", '
            'sceneObjectMaterialIds = [ %s ] }'
        ) % (
            self.sceneObjectMeshId,
            ','.join('"%s"' % mid for mid in self.sceneObjectMaterialIds)
        )


class MeshWriter(object):
    def __init__(self, file_):
        self.file = file_
        self.beginning_written = False
        self.points_written = False
        self.normals_written = False
        self.triangles_written = False

    def begin(self, name, num_vertices, num_triangles):
        self.file.write(
            'MESH {} {} {}\n'
            .format(num_vertices, num_triangles, name))
        self.beginning_written = True

    def write_point(self, vec):
        if not self.beginning_written:
            raise Exception('write the beggining first')
        if self.triangles_written:
            raise Exception(
                'cannot write points once triangles have been written')
        if not self.points_written:
            self.file.write('POINTS\n')
            self.points_written = True
        self.file.write('{} {} {}\n'.format(vec.x1, vec.x2, vec.x3))

    def write_normal(self, vec):
        if not self.normals_written:
            self.file.write('NORMALS\n')
            self.normals_written = True
        self.file.write('{} {} {}\n'.format(vec.x1, vec.x2, vec.x3))

    def write_triangle(self, i1, i2, i3, material_idx, smooth):
        if not self.beginning_written:
            raise Exception('write the beggining first')
        if not self.triangles_written:
            self.file.write('TRIANGLES\n')
            self.triangles_written = True
        self.file.write(
            '{} {} {} {} {}\n'
            .format(i1, i2, i3, material_idx, repr_bool(smooth)))

    def end(self):
        if not self.points_written:
            self.file.write('POINTS\n')
            self.points_written = True
        if not self.normals_written:
            self.file.write('NORMALS\n')
            self.normals_written = True
        if not self.triangles_written:
            self.file.write('TRIANGLES\n')
            self.triangles_written = True
        self.file.write('END MESH\n')


class Vec3(object):
    def __init__(self, x1, x2, x3):
        self.x1 = x1
        self.x2 = x2
        self.x3 = x3

    def repr(self):
        return '(V3 %f %f %f)' % (self.x1, self.x2, self.x3)


def repr_string(s):
    return s.replace('\\', '\\\\').replace('"', '\\"')


def repr_bool(b):
    return 'True' if b else 'False'


def lrepr(x):
    if hasattr(x, 'lrepr'):
        yield from x.lrepr()
    if hasattr(x, 'repr'):
        yield x.repr()
    elif isinstance(x, basestr):
        yield repr_string(x)
    elif isinstance(x, bool):
        yield repr_bool(x)
    elif isinstance(x, list):
        yield from lrepr_list(x)


def lrepr_list(xs):
    yield '['

    try:
        first = next(xs)
    except StopIteration:
        pass
    else:
        yield from lrepr(first)

    for x in xs:
        yield ', '
        yield from lrepr(x)

    yield ']'
