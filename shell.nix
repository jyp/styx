{ nixpkgs ? import <nixpkgs> {} }: 
with (import <nixpkgs> {}).pkgs;
let hp = haskell.haskellPackages.override{
    overrides = self: super: {
      };};
    locpkg = hp.callPackage ./default.nix { }; 
in locpkg.env
