# Use GHC from nixpkgs instead of nixpkgs-unstable, since Hakyll is broken
# in unstable
{ haskellPackages ? (import <nixpkgs> {}).haskellPackages }:
let
  inherit (haskellPackages) cabal hakyll;

in cabal.mkDerivation (self: {
  pname = "personal-site";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  buildDepends = [ hakyll ];
})
