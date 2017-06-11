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

    properties.register()
    ui.register()


def unregister():
    Engine = render_engine.YahrRenderEngine
    bpy.utils.unregister_class(Engine)

    properties.unregister()
    ui.unregister()
