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
        pkgs: with pkgs; {
          nix = mkShell {
            nativeBuildInputs = [
              nixd
              nixfmt-rfc-style
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
