with import <nixpkgs> {}; mkShell {
  nativeBuildInputs = [
    (python312.withPackages (ps: with ps; [
      pygame
    ]))
  ];
}