import trimesh
import numpy as np
from shapely.geometry import Polygon, MultiPolygon

ext_polygon = Polygon([[0, 0], [0, 3], [3, 3], [3, 0]])
int_polygon = Polygon([[1, 1], [2, 1], [2, 2], [1, 2]])
full_polygon = MultiPolygon([ext_polygon, int_polygon])


def extrude_polygon(polygon, height):
    if isinstance(polygon, MultiPolygon):
        meshes = []
        for poly in polygon.geoms:
            meshes.append(trimesh.creation.extrude_polygon(poly, height))
        return trimesh.util.concatenate(meshes)
    return trimesh.creation.extrude_polygon(polygon, height)


mesh = extrude_polygon(full_polygon, 10)
mesh.export("output.stl")
