with import <nixpkgs> { };
mkShell {
  packages = [
    nixd
    statix
    nixfmt

    ruff
    pyright
    (python3.withPackages (ps: with ps; [ ]))
  ];
}
