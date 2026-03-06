with import <nixpkgs> { };
mkShell {
  packages = [
    (python3.withPackages (ps: with ps; [ ]))

    statix
    nixd
    nixfmt-rfc-style

    arduino-cli
    gcc-arm-embedded
  ];
}
