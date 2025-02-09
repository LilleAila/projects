{
  description = "Development environments for various languages";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

    jupyenv = {
      url = "github:tweag/jupyenv";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  # Configured system-wide
  # nixConfig.extra-substituters = [ "https://tweag-jupyter.cachix.org" ];
  # nixConfig.extra-trusted-public-keys = [
  #   "tweag-jupyter.cachix.org-1:UtNH4Zs6hVUFpFBTLaA4ejYavPo5EFFqgd7G7FxGW9g="
  # ];

  outputs =
    { nixpkgs, self, ... }@inputs:
    let
      inherit (nixpkgs) lib;
      systems = lib.systems.flakeExposed;
      pkgsFor = lib.genAttrs systems (system: import nixpkgs { inherit system; });
      forEachSystem = f: lib.genAttrs systems (system: f pkgsFor.${system});
    in
    {
      packages = forEachSystem (pkgs: {
        jupyterlab = inputs.jupyenv.lib.${pkgs.system}.mkJupyterlabNew (
          { ... }:
          {
            inherit nixpkgs;
            imports = [ (import ./jupyterlab.nix) ];
          }
        );
      });

      # idk why it needs to be in apps and cannot just nix run the packages /shrug
      apps = forEachSystem (pkgs: {
        default = {
          program = "${self.packages.${pkgs.system}.jupyterlab}/bin/jupyter-lab";
          type = "app";
        };
        test = {
          program =
            let
              jupyter = pkgs.jupyter-all.override {
                definitions = {
                  python =
                    let
                      pythonPackage = pkgs.python312.withPackages (
                        ps: with ps; [
                          pandas
                          numpy
                          matplotlib

                          ipykernel
                        ]
                      );
                    in
                    pkgs.jupyter-kernel.default.python3
                    // {
                      displayName = "Python kernel";
                      argv = [
                        "${pythonPackage}/bin/python3"
                        "-m"
                        "ipykernel_launcher"
                        "-f"
                        "{connection_file}"
                      ];
                    };
                };
              };
            in
            "${jupyter}/bin/jupyter-lab";
          type = "app";
        };
      });

      devShells = forEachSystem (
        pkgs: with pkgs; rec {
          default = nix;
          nix = mkShell {
            nativeBuildInputs = [
              nixd
              nixfmt-rfc-style
              statix
            ];
          };
          cpp = mkShell {
            nativeBuildInputs = [
              clang
              gcc
              gnumake
              cmake
            ];
          };
          python = mkShell {
            nativeBuildInputs = [
              black
              pyright
            ];
          };
          javascript = mkShell {
            nativeBuildInputs = [
              nodejs
              nodePackages.npm
              nodePackages.typescript-language-server
              nodePackages.typescript
              prettierd
              emmet-ls
              vscode-langservers-extracted
            ];
          };
          astro = mkShell {
            nativeBuildInputs = javascript.nativeBuildInputs ++ [ nodePackages."@astrojs/language-server" ];
          };
          texlive = mkShell {
            nativeBuildInputs = [
              texliveFull
              pandoc
              (python312.withPackages (ps: with ps; [ pygments ]))
            ];
          };
          haskell = mkShell {
            nativeBuildInputs = with haskellPackages; [
              ghc
              haskell-language-server
              ormolu
              ghcid
              stack
              cabal-install
            ];
          };
          lua = mkShell {
            nativeBuildInputs = [
              lua-language-server
              stylua
            ];
          };
          rust = mkShell {
            nativeBuildInputs = [
              rustc
              cargo
              rustfmt
              rust-analyzer
            ];
          };
        }
      );
    };
}
