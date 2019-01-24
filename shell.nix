{ bootstrap ? import <nixpkgs> {} }:

let pkgsSource = fetchTarball "https://github.com/NixOS/nixpkgs-channels/archive/nixos-18.09.tar.gz";
    pkgs = import pkgsSource {};
    hp = pkgs.haskellPackages;
    locpkg = hp.callPackage ./styx/default.nix {};
in locpkg.env
