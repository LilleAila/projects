from OSMPythonTools.nominatim import Nominatim
from OSMPythonTools.overpass import Overpass
from OSMPythonTools.api import Api
import matplotlib.pyplot as plt
from shapely.geometry import Polygon, LineString
import geopandas as gpd

# address = "Lungegårdskaien 40"
# nominatim = Nominatim()
# location = nominatim.query(address)
# assert location is not None, "Location not found!"
# area = location.id()
# print(area)

overpass = Overpass()
api = Api()

bbox = [60.38631650365599, 5.321381765736868, 60.387874780588064, 5.328217556584907]

query = f"""
(
  way["building"]({bbox[0]}, {bbox[1]}, {bbox[2]}, {bbox[3]});
  way["highway"]({bbox[0]}, {bbox[1]}, {bbox[2]}, {bbox[3]});
);
out body;
"""

response = overpass.query(query)
assert response is not None, "No location found"
features = response.toJSON()["elements"]

node_coordinates = {}
node_ids = {node_id for feature in features for node_id in feature["nodes"]}
node_query = f"""
node(id:{",".join(map(str, node_ids))});
out;
"""
node_response = overpass.query(node_query)
assert node_response is not None, "no nodes found?"
for node in node_response.toJSON()["elements"]:
    node_coordinates[node["id"]] = (node["lon"], node["lat"])

building_polygons = []
road_lines = []
for feature in features:
    if "nodes" in feature and feature["type"] == "way":
        try:
            points = [node_coordinates[node_id] for node_id in feature["nodes"]]
            if feature["type"] == "way":
                if "building" in feature.get("tags", {}):
                    building_polygons.append(Polygon(points))
                elif "highway" in feature.get("tags", {}):
                    road_lines.append(LineString(points))
        except KeyError:
            continue

gdf_buildings = gpd.GeoDataFrame(geometry=building_polygons)
gdf_roads = gpd.GeoDataFrame(geometry=road_lines)

fig, ax = plt.subplots(figsize=(10, 10))
gdf_buildings.plot(ax=ax, color="black", edgecolor="white", label="Buildings")
gdf_roads.plot(ax=ax, color="blue", linewidth=0.5, label="Roads")
ax.set_title("Buildings and Roads Map")
ax.set_xlabel("Longitude")
ax.set_ylabel("Latitude")
plt.grid(True)
plt.legend()
plt.show()
