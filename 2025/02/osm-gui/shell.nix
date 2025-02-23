with import <nixpkgs> { };
mkShell {
  packages = [
    black
    pyright
    (python312.withPackages (
      ps: with ps; [
        osmpythontools
        matplotlib
        shapely
        geopandas
        numpy
        trimesh
        mapbox-earcut
        networkx
        scipy
      ]
    ))
  ];
}
