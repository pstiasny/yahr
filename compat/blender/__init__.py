import bpy

from .render_engine import YahrRenderEngine


def register():
    bpy.utils.register_class(YahrRenderEngine)

    from bl_ui import properties_render
    properties_render.RENDER_PT_render.COMPAT_ENGINES.add('yahr_renderer')


def unregister():
    bpy.utils.unregister_class(YahrRenderEngine)

    from bl_ui import properties_render
    properties_render.RENDER_PT_render.COMPAT_ENGINES.remove('yahr_renderer')
