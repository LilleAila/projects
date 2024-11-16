with import <nixpkgs> {}; mkShell {
  nativeBuildInputs = [
    (python311.withPackages (ps: with ps; [
      manim
      manim-slides
      # pyqt5_with_qtmultimedia
    ]))
  ];
}
