{
  lib,
  stdenv,
  fetchFromGitHub,
  autoconf,
  automake,
  pkg-config,
  lttoolbox,
  libxml2,
  icu,
}:
stdenv.mkDerivation rec {
  pname = "apertium";
  version = "3.9.4";

  src = fetchFromGitHub {
    owner = "apertium";
    repo = "apertium";
    rev = "v${version}";
    hash = "sha256-/47A1dYHsFUsIUVPek0pN+bZbANDvwK0elShpIvij78=";
  };

  buildInputs = [
    lttoolbox
    libxml2
    icu
  ];

  nativeBuildInputs = [
    autoconf
    automake
    pkg-config
  ];

  configurePhase = ''
    # chmod +x ./autogen.sh
    ./autogen.sh --prefix $out
  '';

  meta = {
    description = "Core tools (driver script, transfer, tagger, formatters) for the FOSS RBMT system Apertium";
    homepage = "https://github.com/apertium/apertium";
    changelog = "https://github.com/apertium/apertium/blob/${src.rev}/ChangeLog";
    license = with lib.licenses; [
      gpl2Only
      lgpl21Only
    ];
    maintainers = with lib.maintainers; [ LilleAila ];
    mainProgram = "apertium";
    platforms = lib.platforms.all;
  };
}
