with import <nixpkgs> { };
mkShell {
  packages = [
    python312
    # (python3.withPackages (ps: with ps; [
    #   pandas
    #   numpy
    #   trimesh
    #   scipy
    #   pyvista
    # ]))
    black
    pyright
    ruff
  ];
}
