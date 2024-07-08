# Small programming projects

Some small projects organized by year and month with development environments managed by nix and direnv

## Devshell templates
Usage:
```bash
# .envrc
use flake github:LilleAila/projects#nix
use flake github:LilleAila/projects#python
use nix
```
```nix
# shell.nix
{ pkgs ? import <nixpkgs> {} }: with pkgs; mkShell {
  nativeBuildInputs = [
    (python311.withPackages (ps: with ps; [
      matplotlib
      numpy
      tkinter
    ]))
  ];
}
```
