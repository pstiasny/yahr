import bpy


def draw_device(self, context):
    if context.scene.render.engine != 'yahr_renderer':
        return

    layout = self.layout
    layout.prop(context.scene.yahr, 'parallelism_mode')
    layout.prop(context.scene.yahr, 'threads')


def register():
    bpy.types.RENDER_PT_render.append(draw_device)


def unregister():
    bpy.types.RENDER_PT_render.remove(draw_device)
