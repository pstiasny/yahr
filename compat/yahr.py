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
            'Scene { integrator = %s, cullingMode = %s, camera = %s, '
            'materials = [ %s ], lights = [ %s ], objects = [ %s ] }'
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
            'BlinnPhongMaterial { id = "%s", ambient = %s, diffuse = %s, '
            'specular = %s, shininess = %f }'
        ) % (
            repr_string(self.id), self.ambient.repr(), self.diffuse.repr(),
            self.specular.repr(), self.shininess,
        )


class PointLight(object):
    def __init__(self, position):
        self.position = position

    def repr(self):
        return '(PointLight %s)' % self.position.repr()


class TriangleMesh(object):
    def __init__(
            self, triangleMeshPoints, triangleMeshNormals,
            triangleMeshTriangles, triangleMeshSmooth, materialId):
        self.triangleMeshPoints = triangleMeshPoints
        self.triangleMeshNormals = triangleMeshNormals
        self.triangleMeshTriangles = triangleMeshTriangles
        self.triangleMeshSmooth = triangleMeshSmooth
        self.materialId = materialId

    def repr(self):
        return (
            'TriangleMesh {'
            'triangleMeshPoints = [ %s ], '
            'triangleMeshNormals = %s, '
            'triangleMeshTriangles = [ %s ], '
            'triangleMeshSmooth = %s, '
            'materialId = "%s" }'
        ) % (
            ','.join(p.repr() for p in self.triangleMeshPoints),
            self.repr_normals(),
            ','.join(
                '(%d, %d, %d)' % (i1, i2, i3)
                for i1, i2, i3 in self.triangleMeshTriangles),
            self.repr_smooth(),
            repr_string(self.materialId)
        )

    def repr_normals(self):
        if self.triangleMeshNormals is None:
            return 'Nothing'
        else:
            return 'Just [%s]' % (
                ','.join(n.repr() for n in self.triangleMeshNormals)
            )

    def repr_smooth(self):
        if self.triangleMeshSmooth is None:
            return 'Nothing'
        else:
            return 'Just [%s]' % (
                ','.join(repr_bool(s) for s in self.triangleMeshSmooth)
            )


class Vec3(object):
    def __init__(self, x1, x2, x3):
        self.x1 = x1
        self.x2 = x2
        self.x3 = x3

    def repr(self):
        return '(Vec3 %f %f %f)' % (self.x1, self.x2, self.x3)


def repr_string(s):
    return s.replace('\\', '\\\\').replace('"', '\\"')


def repr_bool(b):
    return 'True' if b else 'False'
