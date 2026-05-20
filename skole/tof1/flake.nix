{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  };

  outputs =
    { nixpkgs, self, ... }@inputs:
    let
      inherit (nixpkgs) lib;
      systems = lib.systems.flakeExposed;
      pkgsFor = lib.genAttrs systems (system: import nixpkgs { inherit system; });
      forEachSystem = f: lib.genAttrs systems (system: f pkgsFor.${system});
    in
    {
      devShells = forEachSystem (pkgs: {
        default = pkgs.mkShell {
          packages = with pkgs; [
            nixd
            nixfmt
            statix

    (python3.withPackages (
      ps: with ps; [
        numpy
        sympy
        matplotlib
        pandas
        scipy
        httpagentparser
        seaborn

        ipdb
        pytest
        pytest-html

        pip

        # Run with python3 -m jupyterlab. Don't know which of the three are actually needed.
        jupyter
        jupyterlab
        ipykernel
      ]
    ))

    texliveFull

    ruff
    pyright
    black

    typst
    tinymist
    typstyle
          ];
        };
      });
    };
}
