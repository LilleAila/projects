with import <nixpkgs> {}; mkShell {
  nativeBuildInputs = [
    (python311.withPackages (ps: with ps; [
      bcrypt
      tinydb
      pyotp
    ]))
    mermaid-cli
    plantuml
  ];
}
