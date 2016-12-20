{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where
import System.Process
import Control.Monad.Identity
import Data.Yaml
import Data.Yaml.Config
import qualified Data.Map as M
import Data.Map (Map)
import Data.Aeson.Types (typeMismatch)
import Options.Applicative
import Data.List (intercalate)

defCompil :: [Char]
defCompil = "ghc801"

data Command = Configure


{-
TODO: (not done yet)

- SETUP:
- cabal sandbox init

- for every {local package}
  - find the cabal file
    - add all deps of the cabal file to the list of deps
    - add the local package to the sandbox. (cabal sandbox add-source local-package)

- for every external package
  - do cabal2nix

- add the dependencies + external packages - local packages to the shell

- BUILD:
- nix-shell "cabal install"

- REPL:

- nix-shell "cabal repl ..."

-}

opts :: Options.Applicative.Parser Command
opts = subparser
  (command "configure" (info (pure Configure) (progDesc "Generate .nix files")))

data Repo = Repo {repoLocation :: String}
data Config = Config {cfgExternalPackages :: Map String Repo
                     ,cfgShell :: Maybe ShellConfig
                     ,cfgNixVersion :: Maybe GitVersion}

data ShellConfig = ShellConfig {shellHsPackages :: [String]
                               ,shellExPackages :: [String]}

instance FromJSON Config where
  parseJSON (Object v) = Config <$>
                         v .:? "nix-repos" .!= M.empty  <*> -- list of external repos to depend on
                         v .:? "shell"                  <*> -- Just if not building a package
                         v .:? "nixpkgs"
  parseJSON invalid = typeMismatch "Config" invalid

instance FromJSON Repo where
  parseJSON (Object v) = Repo  <$>
                         v .: "location"               -- location of the repo (in cabal2nix format)
  parseJSON invalid = typeMismatch "Location" invalid

instance FromJSON ShellConfig where
  parseJSON (Object v) = ShellConfig  <$>
                         v .:? "extra-haskell-packages" .!= []  <*>
                         -- list of (extra) haskell packages to put in the shell env.
                         v .:? "extra-external-packages" .!= []
                         -- list of non-haskell packages to put in the shell env.
  parseJSON invalid = typeMismatch "ShellConfig" invalid

data GitVersion = GitVersion {gitCommit :: String, gitSha :: String}
instance FromJSON GitVersion where
  parseJSON (Object v) = GitVersion <$>
                         v .: "commit" <*>
                         v .: "sha256"   -- find here: curl -LO https://nixos.org/channels/nixpkgs-unstable
  parseJSON invalid = typeMismatch "Git version" invalid

main :: IO ()
main = do
  Config{..} <- loadYamlSettings ["styx.yaml"] [] ignoreEnv
  let pkgs = M.assocs cfgExternalPackages
  
  case cfgShell of
    Nothing -> callCommand "cabal2nix . > default.nix"
    _ -> return ()
  forM_ pkgs $ \(n,Repo loc) -> do
    callCommand $ "cabal2nix " ++ loc ++ " > " ++ n ++ ".nix"
  writeFile "shell.nix" $ unlines $
    ["{ nixpkgs ? import <nixpkgs> {}, compiler ? " ++ (show defCompil) ++ " }:"]
    ++ case cfgNixVersion of
      Nothing -> ["let nixpkgs' = nixpkgs;"]
      Just (GitVersion {..}) ->
        ["let nixpkgs_source = nixpkgs.fetchFromGitHub {"
        ,"      owner = \"NixOS\";"
        ,"      repo = \"nixpkgs\";"
        ,"      rev = " ++ show gitCommit ++ ";"
        ,"      sha256 = " ++ show gitSha ++ ";"
        ,"    };"
        ,"    nixpkgs' = (import nixpkgs_source){};"
        ]
    ++ ["in with nixpkgs'.pkgs;"
       ,"let hp = haskell.packages.${compiler}.override{"
       ,"    overrides = self: super: {"
       ]
    ++ ["      " ++ n ++ " = self.callPackage ./" ++ n ++ ".nix {};" | (n,_) <- pkgs]
    ++ ["      };};"]
    ++ case cfgShell of
      Just (ShellConfig {..}) ->
        ["ghc = hp.ghcWithPackages (ps: with ps; ["
        , intercalate " " (map fst pkgs ++ shellHsPackages)
        ,"  ]);"
        ,"in"
        ,"pkgs.stdenv.mkDerivation {"
        ,"  name = \"my-haskell-env-0\";"
        ,"  buildInputs = [ ghc " ++ intercalate " " (map parens shellExPackages) ++ " ];"
        ,"  shellHook = \"eval $(egrep ^export ${ghc}/bin/ghc)\";"
        ,"}"]
      Nothing ->
        ["    locpkg = hp.callPackage ./default.nix { };"
        ,"in locpkg.env"
        ]


parens :: [Char] -> [Char]
parens x = "(" ++ x ++ ")"
