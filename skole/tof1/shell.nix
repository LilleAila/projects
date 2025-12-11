with import <nixpkgs> { };
mkShell {
  packages = [
    (python3.withPackages (
      ps: with ps; [
        numpy
        sympy
        matplotlib
        pandas
        scipy
        httpagentparser

        ipdb
        pytest
        pytest-html

        pip

        # Run with python3 -m jupyterlab. Don't know which of the three are actually needed.
        jupyter
        jupyterlab
        ipykernel
      ]
    ))
  ];
}
