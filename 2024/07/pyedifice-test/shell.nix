with import <nixpkgs> {}; pkgs.mkShell {
  nativeBuildInputs = [
    python311Packages.pyside6
  ];
}
