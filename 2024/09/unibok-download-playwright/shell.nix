with import <nixpkgs> {}; mkShell {
  packages = [
    # (google-chrome.overrideAttrs {
    #   # Bypass the nix unfree stuff
    #   meta.license = lib.licenses.free;
    # })
    ungoogled-chromium
  ];
}
