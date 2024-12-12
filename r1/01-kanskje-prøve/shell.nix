with import <nixpkgs> {}; mkShell {
  packages = [
    (python311.withPackages (ps: with ps; [
      matplotlib
      numpy
    ]))
  ];
}
