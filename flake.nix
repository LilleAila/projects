{
  description = "Development environments for various languages";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  };

  outputs =
    { nixpkgs, ... }@inputs:
    let
      lib = nixpkgs.lib;
      systems = lib.systems.flakeExposed;
      pkgsFor = lib.genAttrs systems (system: import nixpkgs { inherit system; });
      forEachSystem = f: lib.genAttrs systems (system: f pkgsFor.${system});
    in
    {
      devShells = forEachSystem (
        pkgs: with pkgs; rec {
          default = nix;
          nix = mkShell {
            nativeBuildInputs = [
              nixd
              nixfmt-rfc-style
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
