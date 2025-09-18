with import <nixpkgs> { };
mkShell {
  packages = [
    black
    pyright

    (python313.withPackages (
      ps: with ps; [
        numpy
        matplotlib
        scipy
      ]
    ))
  ];
}
