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
          python = mkShell {
            nativeBuildInputs = [
              python3
              black
              nodePackages.pyright
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
          haskell = mkShell {
            nativeBuildInputs = with haskellPackages; [
              ghc
              haskell-language-server
              ormolu
              ghcid
            ];
          };
        }
      );
    };
}
