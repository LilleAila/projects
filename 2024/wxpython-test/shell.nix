with import <nixpkgs> { };
pkgs.mkShell { nativeBuildInputs = [ (python311.withPackages (ps: with ps; [ wxpython ])) ]; }
