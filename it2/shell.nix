with import <nixpkgs> {}; mkShell {
  packages = [
    (python3.withPackages (ps: with ps; [
      numpy
      sympy
      matplotlib
      pandas
      requests
      tkinter
      pillow
      scipy
      httpagentparser
      # sklearn-deap

      # Run with python3 -m jupyterlab. Don't know which of the three are actually needed.
      jupyter
      jupyterlab
      ipykernel
    ]))
  ];
}
