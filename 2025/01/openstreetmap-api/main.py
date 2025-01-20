from OSMPythonTools.nominatim import Nominatim
from OSMPythonTools.overpass import Overpass
from OSMPythonTools.api import Api
import matplotlib.pyplot as plt
from shapely.geometry import Polygon, LineString, box
import geopandas as gpd

overpass = Overpass()
api = Api()


def get_coords(prompt: str) -> list[float]:
    while True:
        try:
            coords = list(map(float, input(prompt).split(", ")))
            if len(coords) == 2:
                return coords
        except ValueError:
            pass
        print("Error. Try again.")


# Southwest, northeast
# bounding_box = [
#     60.38631650365599,
#     5.321381765736868,
#     60.387874780588064,
#     5.328217556584907,
# ]
bounding_box = get_coords("Southwest corner: ") + get_coords("Northeast corner: ")
bounding_box_polygon = box(
    bounding_box[1], bounding_box[0], bounding_box[3], bounding_box[2]
)

query = f"""
(
  way["building"]({bounding_box[0]}, {bounding_box[1]}, {bounding_box[2]}, {bounding_box[3]});
  way["highway"]({bounding_box[0]}, {bounding_box[1]}, {bounding_box[2]}, {bounding_box[3]});
);
out geom;
"""

response = overpass.query(query)
assert response is not None, "No features found!"
features = response.toJSON()["elements"]


building_polygons = []
road_lines = []
walkway_lines = []

for feature in features:
    if "geometry" in feature:
        points = [(node["lon"], node["lat"]) for node in feature["geometry"]]
        if "building" in feature.get("tags", {}):
            building_polygons.append(Polygon(points).intersection(bounding_box_polygon))
        # https://wiki.openstreetmap.org/wiki/Key:highway
        elif "highway" in feature.get("tags", {}):
            highway_type = feature["tags"]["highway"]
            if highway_type in {
                "living_street",
                "pedestrian",
                "track",
                "footway",
                "bridleway",
                "steps",
                "path",
                "sidewalk",
                # "crossing",
                # "traffic_island",
                "cycleway",
            }:
                walkway_lines.append(
                    LineString(points).intersection(bounding_box_polygon)
                )
            elif highway_type in {
                "motorway",
                "trunk",
                "primary",
                "secondary",
                "tertiary",
                "unclassified",
                "residential",
                "motorway_link",
                "trunk_link",
                "primary_link",
                "secondary_link",
                "tertiary_link",
                "service",
                "bus_guideway",
                "escape",
                "raceway",
                "road",
                "busway",
            }:
                road_lines.append(LineString(points).intersection(bounding_box_polygon))

gdf_buildings = gpd.GeoDataFrame(geometry=building_polygons)
gdf_roads = gpd.GeoDataFrame(geometry=road_lines)
gdf_walkways = gpd.GeoDataFrame(geometry=walkway_lines)

fig, ax = plt.subplots(figsize=(10, 10))
gdf_buildings.plot(ax=ax, color="black", edgecolor="white", label="Buildings")
gdf_roads.plot(ax=ax, color="blue", linewidth=0.5, label="Roads")
gdf_walkways.plot(ax=ax, color="green", linewidth=0.5, linestyle="--", label="Walkways")

ax.set_title("Bærgen")
ax.set_xlabel("Longitude")
ax.set_ylabel("Latitude")
plt.legend()
plt.show()
