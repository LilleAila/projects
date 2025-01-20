with import <nixpkgs> { };
mkShell {
  packages = [
    (python312.withPackages (
      ps: with ps; [
        requests
        osmpythontools
        matplotlib
        shapely
        geopandas
      ]
    ))
  ];
}
