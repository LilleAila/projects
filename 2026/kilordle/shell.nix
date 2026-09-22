with import <nixpkgs> { };
mkShell {
  packages = [
    pyright
    ruff
    (python3.withPackages (
      ps: with ps; [
        numpy
        scipy
        matplotlib
        pandas
        pulp

        jupyter
        ipykernel
        jupyterlab
      ]
    ))

    nixd
    statix
    nixfmt
  ];
}
