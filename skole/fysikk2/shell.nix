with import <nixpkgs> { };
mkShell {
  packages = [
    statix
    nixfmt
    nixd

    pyright
    ruff

    typst
    typstyle
    tinymist

    (python3.withPackages (
      ps: with ps; [
        numpy
        sympy
        matplotlib
        pandas
        scipy
        httpagentparser
        seaborn

        jupyterlab
        jupyter
        ipykernel
      ]
    ))
  ];
}
