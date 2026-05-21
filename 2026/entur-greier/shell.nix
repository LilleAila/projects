with import <nixpkgs> { };
mkShell {
  packages = [
    nixd
    nixfmt
    statix

    ruff
    pyright

    (python3.withPackages (
      ps: with ps; [
        numpy
        pandas
        matplotlib
      ]
    ))
  ];
}
