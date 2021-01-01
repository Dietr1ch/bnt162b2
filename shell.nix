{ pkgs ? import <nixpkgs> { } }:

with pkgs;

mkShell {
  buildInputs = [
    # Nix
    nixpkgs-fmt

    # go
    go

    # python
    python39
    pythonPackages.pip

    # haskell
    ghc
    cabal-install
  ];

  shellHook = ''
    RUST_BACKTRACE=0
  '';
}
