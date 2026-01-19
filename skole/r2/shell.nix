with import <nixpkgs> { };
mkShell {
  packages = [
    black
    pyright
    ruff

    gnuplot

    (python313.withPackages (
      ps: with ps; [
        numpy
        matplotlib
        scipy
        pandas
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
