# %%
import pymeshlab
ms = pymeshlab.MeshSet()
ms.load_new_mesh('RAJA2-simplified.obj')
ms.select_faces_by_color(color=pymeshlab.Color(255,0,255),colorspace='RGB')
ms.select_connected_faces()
ms.vertex_color_filling(color1=pymeshlab.Color(0,0,0),onselected=True)
ms.save_current_mesh("out.obj")
# %%
