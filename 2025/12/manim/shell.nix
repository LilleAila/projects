with import <nixpkgs> {}; mkShell {
  packages = [
    black
    pyright
    texliveFull
  ];
}
