{ pkgs, ... }:
{
  jupyterlab.runtimePackages = with pkgs.python312Packages; [
    pandas
  ];
  kernel.python.test = {
    enable = true;
    requiredRuntimePackages = with pkgs.python312Packages; [
      pandas
    ];
    # extraPackages =
    #   ps: with ps; [
    #     # numpy
    #     # sympy
    #     # matplotlib
    #     pandas
    #     # requests
    #   ];
  };
}
