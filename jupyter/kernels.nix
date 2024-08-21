{ pkgs, ... }:
{
  kernel.python.minimal = {
    enable = true;
    extraPackages =
      ps: with ps; [
        numpy
        matplotlib
      ];
  };
}
