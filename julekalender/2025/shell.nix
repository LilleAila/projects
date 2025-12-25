with import <nixpkgs> { };
mkShell {
  packages = [
    ghc
    haskell-language-server
    ormolu
    ghcid
    black
    pyright
    ruff
    (python3.withPackages (ps: with ps; [
      pillow
      numpy
    ]))
    zsteg
  ];
}
