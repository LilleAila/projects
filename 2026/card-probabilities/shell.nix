with import <nixpkgs> { };
mkShell {
  packages = [
    nixd
    statix
    nixfmt

    pyright
    ruff
    (python3.withPackages (
      ps: with ps; [
        matplotlib
        scipy
        numpy
      ]
    ))
  ];
}
