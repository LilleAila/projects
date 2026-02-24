with import <nixpkgs> { };
mkShell {
  packages = [
    pyright
    ruff
    (python313.withPackages (
      ps: with ps; [
        numpy
        scipy
        matplotlib
        plotly
        pyscf
        pyvista
      ]
    ))

    nixfmt-rfc-style
    nixd
    statix
  ];
}
