with import <nixpkgs> {}; mkShell {
  nativeBuildInputs = [
    (python311.withPackages (ps: with ps; [
      manim
      manim-slides
      pyqt6
      qtpy
    ]))
    libsForQt5.qt5.qtmultimedia
  ];
}
