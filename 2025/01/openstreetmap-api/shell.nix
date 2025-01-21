with import <nixpkgs> { };
mkShell {
  packages = [
    (python312.withPackages (
      ps: with ps; [
        osmpythontools
        matplotlib
        shapely
        geopandas
        numpy
        trimesh
        mapbox-earcut
      ]
    ))
  ];
}
