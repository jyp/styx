{ bootstrap ? import <nixpkgs> {} }:

let pkgsSource = fetchTarball "https://github.com/NixOS/nixpkgs/archive/nixos-21.05.tar.gz";
    pkgs = import pkgsSource {};
    hp = pkgs.haskellPackages;
    locpkg = hp.callPackage ./styx/default.nix {};
in locpkg.env
