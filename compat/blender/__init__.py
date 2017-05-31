# required for the reload functionality
if "bpy" in locals():
    import importlib
    if "render_engine" in locals():
        importlib.reload(render_engine)
    if "ui" in locals():
        importlib.reload(ui)
    if "properties" in locals():
        importlib.reload(properties)

import bpy

from . import properties, render_engine, ui


def register():
    Engine = render_engine.YahrRenderEngine
    bpy.utils.register_class(Engine)

    from bl_ui import properties_render
    properties_render.RENDER_PT_render.COMPAT_ENGINES.add(Engine.bl_idname)
    properties_render.RENDER_PT_dimensions.COMPAT_ENGINES.add(Engine.bl_idname)
    properties.register()
    ui.register()


def unregister():
    Engine = render_engine.YahrRenderEngine
    bpy.utils.unregister_class(Engine)

    from bl_ui import properties_render
    properties_render.RENDER_PT_render.COMPAT_ENGINES.remove(Engine.bl_idname)
    properties_render.RENDER_PT_dimensions.COMPAT_ENGINES.remove(Engine.bl_idname)
    properties.unregister()
    ui.unregister()
