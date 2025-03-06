with import <nixpkgs> {}; mkShell {
  packages = [
    (python311.withPackages (ps: with ps; [
      numpy
      matplotlib
      pandas
      requests
      tkinter
      pillow

      # Run with python3 -m jupyterlab. Don't know which of the three are actually needed.
      jupyter
      jupyterlab
      ipykernel
    ]))
  ];
}
