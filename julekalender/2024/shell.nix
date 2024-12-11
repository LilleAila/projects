with import <nixpkgs> { };
mkShell {
  packages = [
    (python312.withPackages (
      ps: with ps; [
        pillow
        numpy
        matplotlib
      ]
    ))
  ];
}
