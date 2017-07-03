import bpy


def draw_device(self, context):
    if context.scene.render.engine != 'yahr_renderer':
        return

    layout = self.layout
    layout.prop(context.scene.yahr, 'parallelism_mode')
    layout.prop(context.scene.yahr, 'threads')


def register():
    bpy.types.RENDER_PT_render.append(draw_device)

    from .render_engine import YahrRenderEngine as Engine
    from bl_ui import properties_render, properties_material, properties_data_lamp
    properties_render.RENDER_PT_render.COMPAT_ENGINES.add(Engine.bl_idname)
    properties_render.RENDER_PT_dimensions.COMPAT_ENGINES.add(Engine.bl_idname)
    properties_material.MATERIAL_PT_context_material.COMPAT_ENGINES.add(Engine.bl_idname)
    properties_material.MATERIAL_PT_diffuse.COMPAT_ENGINES.add(Engine.bl_idname)
    properties_material.MATERIAL_PT_specular.COMPAT_ENGINES.add(Engine.bl_idname)
    properties_data_lamp.DATA_PT_lamp.COMPAT_ENGINES.add(Engine.bl_idname)

def unregister():
    bpy.types.RENDER_PT_render.remove(draw_device)

    from .render_engine import YahrRenderEngine as Engine
    from bl_ui import properties_render, properties_material, properties_data_lamp
    properties_render.RENDER_PT_render.COMPAT_ENGINES.remove(Engine.bl_idname)
    properties_render.RENDER_PT_dimensions.COMPAT_ENGINES.remove(Engine.bl_idname)
    properties_material.MATERIAL_PT_context_material.COMPAT_ENGINES.remove(Engine.bl_idname)
    properties_material.MATERIAL_PT_diffuse.COMPAT_ENGINES.remove(Engine.bl_idname)
    properties_material.MATERIAL_PT_specular.COMPAT_ENGINES.remove(Engine.bl_idname)
    properties_data_lamp.DATA_PT_lamp.COMPAT_ENGINES.remove(Engine.bl_idname)
