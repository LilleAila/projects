with import <nixpkgs> { };
mkShell {
  packages = [
    nixd
    nixfmt-rfc-style
    statix

    ruff
    pyright
    (python3.withPackages (
      ps: with ps; [
        pip
      ]
    ))
  ];
}
