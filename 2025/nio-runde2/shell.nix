with import <nixpkgs> { };
mkShell {
  packages = [
    cargo
    rustc
    rust-analyzer
    nixd
    nixfmt-rfc-style
    statix
    libgcc
  ];
}
