{ pkgs, ... }:
{
  kernel.python.default = {
    enable = true;
    extraPackages =
      ps: with ps; [
        # numpy
        # sympy
        # matplotlib
        # pandas
        # requests
      ];
  };
}
