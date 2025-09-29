with import <nixpkgs> { };
mkShell {
  packages = [
    black
    pyright

    (python3.withPackages (
      ps: with ps; [
        numpy
        matplotlib
      ]
    ))
  ];
}
