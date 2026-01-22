# ruff: noqa: F401

# This worked, but it generated just the front of the scan which was possible to import into blender, but from there i was not able to create a good model from it.

import sys
import pandas as pd
import numpy as np
from scipy.spatial import Delaunay
import trimesh
import pyvista as pv
import tempfile
import open3d.cpu.pybind as o3d

assert len(sys.argv) == 2, "Incorrect number of arguments provided"

df = pd.read_csv(sys.argv[1], header=None, names=["x", "y", "z", "intensity"])

threshold = 50
points = np.array(df[df["intensity"] > threshold][["x", "y", "z"]])

# With scipy
# tri = Delaunay(points)
# mesh = trimesh.Trimesh(vertices=points, faces=tri.simplices)
#
# mesh.export("mesh.obj")

# With pyvista
# cloud = pv.PolyData(points)
# mesh = cloud.delaunay_3d(alpha=1.0)
# surface = mesh.extract_surface()
# surface.save("mesh.obj")
#
# plotter = pv.Plotter()
# plotter.add_mesh(surface, color="lightblue")
# plotter.show()


pcd = o3d.geometry.PointCloud()
pcd.points = o3d.utility.Vector3dVector(points)

pcd.estimate_normals(search_param=o3d.geometry.KDTreeSearchParamHybrid(radius=0.05, max_nn=30))
pcd.orient_normals_consistent_tangent_plane(30)

mesh, densities = o3d.geometry.TriangleMesh.create_from_point_cloud_poisson(pcd, depth=9)

densities = np.asarray(densities)
vertices_to_keep = densities > np.quantile(densities, 0.05)
mesh = mesh.select_by_index(np.where(vertices_to_keep)[0])

o3d.io.write_triangle_mesh("mesh.ply", mesh)
# o3d.visualization.draw_geometries([mesh])
