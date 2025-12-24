with import <nixpkgs> { };
mkShell {
  packages = [
    ghc
    haskell-language-server
    ormolu
    ghcid
    black
    pyright
    (python3.withPackages (ps: with ps; [
      pillow
    ]))
    zsteg
  ];
}
