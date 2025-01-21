from OSMPythonTools.overpass import Overpass
import matplotlib.pyplot as plt
from shapely.geometry import Polygon, MultiPolygon, LineString, box
import geopandas as gpd
import trimesh


class AreaMap:
    def __init__(self) -> None:
        self.overpass = Overpass()
        self.bounding_box = None
        self.bounding_box_polygon = None
        self.buildings = None
        self.roads = None
        self.walkways = None

    def get_coords(self, prompt: str) -> list[float]:
        while True:
            try:
                coords = list(map(float, input(prompt).split(", ")))
                if len(coords) == 2:
                    return coords
            except ValueError:
                pass
            print("Error. Try again.")

    def set_utm_epsg(self):
        assert self.bounding_box is not None, "No bounding box set!"
        south_lat, west_lon, north_lat, east_lon = self.bounding_box
        lat = (south_lat + north_lat) / 2
        lon = (west_lon + east_lon) / 2
        zone = int((lon + 180) / 6) + 1
        northern = lat >= 0
        self.epsg = 32600 + zone if northern else 32700 + zone

    def set_bounding_box_interactive(self) -> None:
        print("Copy the coordinates of the corners, and paste them here.")
        southwest = self.get_coords("Southwest corner: ")
        northeast = self.get_coords("Southeast corner: ")
        self.bounding_box = southwest + northeast
        # Example for testing
        # self.bounding_box = [
        #     60.38631650365599,
        #     5.321381765736868,
        #     60.387874780588064,
        #     5.328217556584907,
        # ]
        self.bounding_box_polygon = box(
            self.bounding_box[1],
            self.bounding_box[0],
            self.bounding_box[3],
            self.bounding_box[2],
        )
        self.set_utm_epsg()

    def get_features(self):
        bb = self.bounding_box
        assert bb is not None
        query = f"""
        (
          way["building"]({bb[0]}, {bb[1]}, {bb[2]}, {bb[3]});
          way["highway"]({bb[0]}, {bb[1]}, {bb[2]}, {bb[3]});
        );
        out geom;
        """

        response = self.overpass.query(query)
        assert response is not None, "No features found!"
        return response.toJSON()["elements"]

    def build_map(self) -> None:
        features = self.get_features()

        building_polygons = []
        road_lines = []
        walkway_lines = []

        for feature in features:
            if "geometry" in feature:
                points = [(node["lon"], node["lat"]) for node in feature["geometry"]]
                if "building" in feature.get("tags", {}):
                    building_polygons.append(
                        Polygon(points).intersection(self.bounding_box_polygon)
                    )
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
                            LineString(points).intersection(self.bounding_box_polygon)
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
                        road_lines.append(
                            LineString(points).intersection(self.bounding_box_polygon)
                        )

        # 4326 is lat/lon
        self.buildings = gpd.GeoDataFrame(geometry=building_polygons, crs="EPSG:4326").to_crs(epsg=self.epsg)
        self.roads = gpd.GeoDataFrame(geometry=road_lines, crs="EPSG:4326").to_crs(epsg=self.epsg)
        self.walkways = gpd.GeoDataFrame(geometry=walkway_lines, crs="EPSG:4326").to_crs(epsg=self.epsg)

    def plot_map(self) -> None:
        _, ax = plt.subplots(figsize=(10, 10))
        assert (
            self.buildings is not None
            and self.roads is not None
            and self.walkways is not None
        )
        self.buildings.plot(ax=ax, color="black", edgecolor="white", label="Buildings")
        self.roads.plot(ax=ax, color="blue", linewidth=0.5, label="Roads")
        self.walkways.plot(ax=ax, color="green", linewidth=0.5, label="Walkways")
        plt.show()


class AreaMap3D(AreaMap):
    def __init__(self):
        super().__init__()

    def extrude_polygon(self, polygon, height):
        if isinstance(polygon, MultiPolygon):
            meshes = [
                trimesh.creation.extrude_polygon(poly, height)
                for poly in polygon.geoms if not poly.is_empty
            ]
            return trimesh.util.concatenate(meshes)
        elif polygon.is_empty:
            raise ValueError("Empty polygon!")
        return trimesh.creation.extrude_polygon(polygon, height)

    def extrude_polygons(self, polygons, height):
        extruded = []
        for polygon in polygons.geometry:
            mesh = self.extrude_polygon(polygon, height)
            extruded.append(mesh)
        return trimesh.util.concatenate(extruded)

    def extrude_lines(self, lines, width, height):
        extruded = []
        for line in lines.geometry:
            polygon = line.buffer(width / 2, cap_style=2)
            mesh = self.extrude_polygon(polygon, height)
            extruded.append(mesh)
        return trimesh.util.concatenate(extruded)

    def create_base_plate(self, height=2, wall_width=2, wall_height=5):
        assert self.bounding_box_polygon is not None, "Bounding box is not set!"
        gdf = gpd.GeoDataFrame(geometry=[self.bounding_box_polygon], crs="EPSG:4326").to_crs(epsg=self.epsg)
        assert gdf is not None, "Invalid bounding box!"
        base_plate = gdf.geometry[0]

        outer_wall = base_plate.buffer(wall_width, join_style=2)
        wall = outer_wall.difference(base_plate)

        meshes = [
            self.extrude_polygon(base_plate, -height),
            self.extrude_polygon(wall, height + wall_height).apply_translation([0, 0, -height])
        ]

        return trimesh.util.concatenate(meshes)

    def export_stl(self, output_file):
        meshes = [
            self.create_base_plate(4, 5, 14),
            self.extrude_polygons(self.buildings, 24),
            self.extrude_lines(self.roads, 6, 5),
            self.extrude_lines(self.walkways, 4, 3),
        ]
        mesh = trimesh.util.concatenate(meshes)
        mesh.export(output_file)



if __name__ == "__main__":
    area_map = AreaMap3D()
    area_map.set_bounding_box_interactive()
    area_map.build_map()
    # area_map.plot_map()
    area_map.export_stl("output.stl")
