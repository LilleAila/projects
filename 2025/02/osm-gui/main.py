from tkinter import filedialog, messagebox
import tkinter as tk

from OSMPythonTools.overpass import Overpass, Nominatim

from shapely.geometry import Polygon, MultiPolygon, LineString, box
from matplotlib.backends.backend_tkagg import FigureCanvasTkAgg
import matplotlib.pyplot as plt
import shapely.affinity as aff
import geopandas as gpd
import trimesh

import pandas as pd
import numpy as np
import math


class AreaMap:
    def __init__(
        self,
        map_size=200,
        map_offset_north=0,
        map_offset_east=0,
        size=140,
        walkway_width=2,
        road_width=3,
    ) -> None:
        self.nominatim = Nominatim()

        self.overpass = Overpass()
        self.bounding_box = None
        self.bounding_box_polygon = None
        self.buildings = None
        self.roads = None
        self.walkways = None

        # Meters, from real world map
        self.map_size = map_size
        self.map_offset_north = map_offset_north
        self.map_offset_east = map_offset_east
        self.walkway_width = walkway_width
        self.road_width = road_width

        # Millimeters, in the result
        self.size = size

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
        northeast = self.get_coords("Northeast corner: ")
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

    def set_bounding_square(self, address) -> None:
        location = self.nominatim.query(address)
        if not location or len(location.toJSON()) == 0:
            raise ValueError(f'Location "{address}" not found!')
        coords = location.toJSON()[0]
        lat, lon = float(coords["lat"]), float(coords["lon"])

        lat_to_meters, lon_to_meters = self.meters_at_coord(lat)
        delta_lat, delta_lon = (
            self.map_size / lat_to_meters,
            self.map_size / lon_to_meters,
        )

        offset_delta_lat, offset_delta_lon = (
            self.map_offset_north / lat_to_meters,
            self.map_offset_east / lon_to_meters,
        )
        lat += offset_delta_lat
        lon += offset_delta_lon

        self.bounding_box = [
            lat - delta_lat,
            lon - delta_lon,
            lat + delta_lat,
            lon + delta_lon,
        ]

        self.bounding_box_polygon = box(
            self.bounding_box[1],
            self.bounding_box[0],
            self.bounding_box[3],
            self.bounding_box[2],
        )
        self.set_utm_epsg()

    def set_bounding_square_interactive(self):
        while True:
            try:
                self.set_bounding_square(input("Address: "))
                break
            except EOFError:
                print(":(")
                exit()
            except ValueError:
                continue

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

    def normalize_geometry(self, combined_geometry, gdf):
        min_rect = combined_geometry.minimum_rotated_rectangle
        x, y = min_rect.exterior.coords.xy
        dx = x[1] - x[0]
        dy = y[1] - y[0]
        angle = -np.arctan2(dy, dx)

        centroid = combined_geometry.centroid

        # gdf["geometry"] = gdf["geometry"].apply(
        #     lambda geom: aff.rotate(geom, angle, origin=centroid, use_radians=True)
        # )

        minx, miny, maxx, maxy = gdf.total_bounds
        width, height = maxx - minx, maxy - miny

        scale_factor = self.size / max(width, height)
        gdf["geometry"] = gdf["geometry"].apply(
            lambda geom: aff.scale(geom, scale_factor, scale_factor, origin=centroid)
        )

        minx, miny, _, _ = gdf.total_bounds
        gdf["geometry"] = gdf["geometry"].apply(
            lambda geom: aff.translate(geom, xoff=-minx, yoff=-miny)
        )

        return gdf

    def meters_at_coord(self, lat):
        lat_to_meters = 111320
        lon_to_meters = lat_to_meters * math.cos(math.radians(lat))
        return lat_to_meters, lon_to_meters

    def meters_to_degrees(self, meters: float) -> float:
        # Approximation
        assert self.bounding_box is not None, "Bounding box is not set!"
        south_lat, _, north_lat, _ = self.bounding_box
        avg_lat = (south_lat + north_lat) / 2
        lat_to_meters, lon_to_meters = self.meters_at_coord(avg_lat)
        lat_degrees = meters / lat_to_meters
        lon_degrees = meters / lon_to_meters
        return (lat_degrees + lon_degrees) / 2

    def build_map(self) -> None:
        features = self.get_features()

        building_polygons = []
        road_lines = []
        walkway_lines = []

        walkway_width = self.meters_to_degrees(self.walkway_width)
        road_width = self.meters_to_degrees(self.road_width)

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
                            LineString(points)
                            .buffer(walkway_width / 2, cap_style="flat")
                            .intersection(self.bounding_box_polygon)
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
                        # "cycleway",
                    }:
                        road_lines.append(
                            LineString(points)
                            .buffer(road_width / 2, cap_style="flat")
                            .intersection(self.bounding_box_polygon)
                        )

        # 4326 is lat/lon
        self.buildings = gpd.GeoDataFrame(
            geometry=building_polygons, crs="EPSG:4326"
        ).to_crs(epsg=self.epsg)
        self.roads = gpd.GeoDataFrame(geometry=road_lines, crs="EPSG:4326").to_crs(
            epsg=self.epsg
        )
        self.walkways = gpd.GeoDataFrame(
            geometry=walkway_lines, crs="EPSG:4326"
        ).to_crs(epsg=self.epsg)

    def get_combined_gdf(self):
        return pd.concat(
            [self.buildings.geometry, self.roads.geometry, self.walkways.geometry]
        )

    def normalize_geometries(self):
        combined_geometry = self.get_combined_gdf().union_all()
        self.buildings = self.normalize_geometry(combined_geometry, self.buildings)
        self.roads = self.normalize_geometry(combined_geometry, self.roads)
        self.walkways = self.normalize_geometry(combined_geometry, self.walkways)

    def plot_map(self, ax) -> None:
        assert (
            self.buildings is not None
            and self.roads is not None
            and self.walkways is not None
        )
        self.buildings.plot(ax=ax, color="black", edgecolor="white", label="Buildings")
        self.roads.plot(ax=ax, color="blue", linewidth=0.5, label="Roads")
        self.walkways.plot(ax=ax, color="green", linewidth=0.5, label="Walkways")


class AreaMap3D(AreaMap):
    def __init__(
        self,
        base_plate_height=1.5,
        wall_width=1.5,
        wall_height=2.5,
        buildings_height=5,
        roads_height=2,
        walkways_height=1,
        **kwargs,
    ):
        super().__init__(**kwargs)
        # In millimeters
        self.base_plate_height = base_plate_height
        self.wall_width = wall_width
        self.wall_height = wall_height
        self.buildings_height = buildings_height
        self.roads_height = roads_height
        self.walkways_height = walkways_height

        self.size = self.size - self.wall_width * 2

    def extrude_polygon(self, polygon, height):
        if isinstance(polygon, MultiPolygon):
            meshes = [
                trimesh.creation.extrude_polygon(poly, height)
                for poly in polygon.geoms
                if not poly.is_empty
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

    def create_base_plate(self):
        minx, miny, maxx, maxy = self.get_combined_gdf().total_bounds
        base_plate = box(minx, miny, maxx, maxy)
        outer_wall = base_plate.buffer(self.wall_width, join_style="mitre")
        wall = outer_wall.difference(base_plate)

        meshes = [
            self.extrude_polygon(base_plate, -self.base_plate_height),
            self.extrude_polygon(
                wall, self.base_plate_height + self.wall_height
            ).apply_translation([0, 0, -self.base_plate_height]),
        ]

        return trimesh.util.concatenate(meshes)

    def export_stl(self, output_file):
        meshes = [
            self.create_base_plate(),
            self.extrude_polygons(self.buildings, self.buildings_height),
            self.extrude_polygons(self.roads, self.roads_height),
            self.extrude_polygons(self.walkways, self.walkways_height),
        ]
        mesh = trimesh.util.concatenate(meshes)
        mesh.export(output_file)

    def export_obj(self, output_file):
        if not output_file:
            raise ValueError("No output file provided!")

        base_plate_mesh = self.create_base_plate()
        buildings_mesh = self.extrude_polygons(self.buildings, self.buildings_height)
        roads_mesh = self.extrude_polygons(self.roads, self.roads_height)
        walkways_mesh = self.extrude_polygons(self.walkways, self.walkways_height)

        base_plate_mesh.visual.face_colors = [255, 255, 0, 255]  # base plate: yellow
        buildings_mesh.visual.face_colors = [255, 0, 0, 255]  # buildings: red
        roads_mesh.visual.face_colors = [0, 255, 0, 255]  # roads: green
        walkways_mesh.visual.face_colors = [0, 0, 255, 255]  # walkway: blue

        meshes = [
            base_plate_mesh,
            buildings_mesh,
            roads_mesh,
            # walkways_mesh,
        ]
        trimesh.util.concatenate(meshes).export(output_file, file_type="obj")


class OsmApp:
    def __init__(self, root):
        self.root = root
        self.root.title("OpenStreetMap 3d Model Generation")
        self.root.protocol("WM_DELETE_WINDOW", self.quit)

        self.areaMap = AreaMap3D()

        self.create_gui()

    def quit(self):
        self.root.quit()
        self.root.destroy()

    def create_gui(self):
        left_frame = tk.Frame(self.root)
        left_frame.pack(side=tk.LEFT, padx=10, pady=10, fill=tk.Y)

        self.address = tk.StringVar()
        self.address_input = tk.Entry(left_frame, textvariable=self.address, width=30)
        self.address_input.pack(pady=5)

        button_frame = tk.Frame(left_frame)
        button_frame.pack(pady=5)

        reset_button = tk.Button(button_frame, text="Reset", command=self.reset)
        reset_button.pack(side=tk.LEFT, padx=5)

        preview_button = tk.Button(
            button_frame, text="Preview", command=self.update_preview
        )
        preview_button.pack(side=tk.LEFT, padx=5)

        export_button = tk.Button(
            button_frame, text="Export", command=self.export_model
        )
        export_button.pack(side=tk.LEFT, padx=5)

        self.file_path = tk.StringVar()
        file_path_button = tk.Button(
            left_frame, text="Select output file", command=self.select_output
        )
        file_path_button.pack(pady=5)
        file_path_label = tk.Label(
            left_frame, textvariable=self.file_path, width=30, wraplength=200
        )
        file_path_label.pack(pady=5)

        right_frame = tk.Frame(self.root)
        right_frame.pack(side=tk.RIGHT, padx=10, pady=10, fill=tk.BOTH, expand=True)

        self.fig, self.ax = plt.subplots()
        self.canvas = FigureCanvasTkAgg(self.fig, master=right_frame)
        self.canvas.get_tk_widget().pack(fill=tk.BOTH, expand=True)

        self.reset()

    def select_output(self):
        file_path = filedialog.asksaveasfilename(
            title="Select output file",
            defaultextension=".obj",
            filetypes=(("OBJ files", "*.obj"), ("All files", "*.*")),
        )

        if file_path:
            self.file_path.set(file_path)

    def reset_preview(self):
        self.ax.clear()
        self.ax.set_xticks([])
        self.ax.set_yticks([])
        for spine in self.ax.spines.values():
            spine.set_visible(False)
        self.canvas.draw()

    def reset(self):
        self.reset_preview()
        self.address.set("")
        self.file_path.set("")

    def create_map(self):
        try:
            self.areaMap.set_bounding_square(self.address.get())
        except ValueError as e:
            messagebox.showwarning("Warning", str(e))
            self.reset()
        self.areaMap.build_map()
        self.areaMap.normalize_geometries()

    def update_preview(self):
        self.reset_preview()
        self.create_map()
        self.areaMap.plot_map(self.ax)
        self.canvas.draw()

    def export_model(self):
        self.update_preview()
        try:
            self.areaMap.export_obj(self.file_path.get())
            messagebox.showinfo("Info", f"Exported map to {self.file_path.get()}")
        except ValueError as e:
            messagebox.showwarning("Warning", str(e))


if __name__ == "__main__":
    root = tk.Tk()
    app = OsmApp(root)
    root.mainloop()
