with import <nixpkgs> { };
mkShell {
  packages = [
    (python312.withPackages (ps: with ps; [
        numpy
        scipy
        mido
        librosa
      ]
    ))
  ];
}