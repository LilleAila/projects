with import <nixpkgs> { };
mkShell {
  packages = [
    black
    pyright

    gnuplot

    (python313.withPackages (
      ps: with ps; [
        numpy
        matplotlib
        scipy
      ]
    ))

    (ghc.withPackages (ps: with ps; [
      gnuplot
    ]))
    haskell-language-server
    ormolu
    ghcid
    stack
    cabal-install
  ];
}
