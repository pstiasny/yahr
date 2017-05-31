import bpy
from bpy.props import (
    EnumProperty,
    IntProperty,
    PointerProperty,
)


enum_parallelism_mode = (
    ('DISABLED', 'Disabled', 'No parallelism'),
    ('EVAL', 'Eval', 'Use the Eval monad for parallel execution'),
)


class YahrRenderSettings(bpy.types.PropertyGroup):
    @classmethod
    def register(cls):
        bpy.types.Scene.yahr = PointerProperty(
            name='Yahr Render Settings',
            description='Yahr render settings',
            type=cls)
        cls.threads = IntProperty(
            name='Threads',
            description='Number of parallel threads',
            default=1, min=1)
        cls.parallelism_mode = EnumProperty(
            name='Parallelism mode',
            description='Parallelism mode to run Yahr in',
            items=enum_parallelism_mode,
            default='EVAL')


def register():
    bpy.utils.register_class(YahrRenderSettings)


def unregister():
    bpy.utils.unregister_class(YahrRenderSettings)
