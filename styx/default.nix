{ mkDerivation, aeson, base, containers, directory, filepath, mtl
, optparse-applicative, process, stdenv, text, yaml
}:
mkDerivation {
  pname = "styx";
  version = "1.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson base containers directory filepath mtl optparse-applicative
    process text yaml
  ];
  description = "A generator of nix files";
  license = "GPL";
}
