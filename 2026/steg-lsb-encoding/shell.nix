with import <nixpkgs> { };
mkShell {
  packages = [
    ruff
    pyright

    (python3.withPackages (ps: with ps; [
      opencv4
      numpy
    ]))
  ];
}
