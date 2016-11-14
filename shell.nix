{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc801" }: 
with (import <nixpkgs> {}).pkgs;
let hp = haskell.packages.${compiler}.override{
    overrides = self: super: {
      };};
    locpkg = hp.callPackage ./default.nix { }; 
in locpkg.env
