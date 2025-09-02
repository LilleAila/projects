with import <nixpkgs> {}; mkShell {
  packages = [
    (python3.withPackages (ps: with ps; [
      openpyxl
      pillow

      ipdb
    ]))
  ];
}
