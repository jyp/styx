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
import System.Directory (createDirectoryIfMissing)
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
data Config =
  Config {cfgNixpkgsVersion :: Maybe GitVersion
         ,cfgLocalPackages :: Map String Repo -- list of local packages (must be on the local filesystem)
         ,cfgExternalSourceDeps :: Map String Repo -- mapping of package names to locations as understood by cabal2nix
         ,cfgNixHsDeps :: [String]
         -- ^ haskell deps to fetch directly from nix (usually empty for a cabal project, as the cabal file will specifiy deps)
         ,cfgNixOtherDeps :: [String]
         -- ^ Other nix dependencies (non haskell packages)
         }

data ShellConfig = ShellConfig {}

instance FromJSON Config where
  parseJSON (Object v) = Config <$>
                         v .:? "nixpkgs" <*>
                         v .:? "local-packages" .!= M.empty  <*>
                         v .:? "source-deps" .!= M.empty  <*>
                         v .:? "nix-deps" .!= []  <*>
                         v .:? "non-haskell-deps" .!= []
  parseJSON invalid = typeMismatch "Config" invalid

instance FromJSON Repo where
  parseJSON (Object v) = Repo  <$>
                         v .: "location"               -- location of the repo (in cabal2nix format)
  parseJSON invalid = typeMismatch "Location" invalid

data GitVersion = GitVersion {gitCommit :: String, gitSha :: String}
instance FromJSON GitVersion where
  parseJSON (Object v) = GitVersion <$>
                         v .: "commit" <*>
                         v .: "sha256"   -- find here: curl -LO https://nixos.org/channels/nixpkgs-unstable
  parseJSON invalid = typeMismatch "Git version" invalid

locToNix :: String -> Repo -> IO ()
locToNix p (Repo loc) = do
    callCommand $ "cabal2nix " ++ loc ++ " > .styx/" ++ p ++ ".nix"

main :: IO ()
main = do
  Config{..} <- loadYamlSettings ["styx.yaml"] [] ignoreEnv
  createDirectoryIfMissing False ".styx"
  callCommand "cabal sandbox init --sandbox .styx"
  forM_ (M.assocs cfgLocalPackages) (uncurry locToNix)
  forM_ (M.assocs cfgExternalSourceDeps) (uncurry locToNix)
  writeFile ".styx/shell.nix" $ unlines $
    ["{ nixpkgs ? import <nixpkgs> {}, compiler ? " ++ (show defCompil) ++ " }:"]
    ++ case cfgNixpkgsVersion of
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
    ++ ["      " ++ n ++ " = self.callPackage ./" ++ n ++ ".nix {};"
       | n <- (M.keys cfgExternalSourceDeps ++ M.keys cfgLocalPackages)]
    ++ ["      };};"
       ,"     getHaskellDeps = ps: path:"
       ,"        let f = import path;"
       ,"            gatherDeps = {buildDepends ? [], libraryHaskellDepends ? [], executableHaskellDepends ? [], ...}:"
       ,"               libraryHaskellDepends ++ executableHaskellDepends;"
       ,"            x = f (builtins.intersectAttrs (builtins.functionArgs f) ps // {stdenv = stdenv; mkDerivation = gatherDeps;});"
       ,"        in x;"
       ,"ghc = hp.ghcWithPackages (ps: with ps; subtractLists"
       , "[" ++ intercalate " " (M.keys cfgLocalPackages) ++ "]"
       , "(["
       , intercalate " " (M.keys cfgExternalSourceDeps ++ cfgNixHsDeps)
       ,"  ] " ++ concat [" ++ getHaskellDeps ps ./" ++ n | n <- M.keys cfgLocalPackages] ++ "));"
       ,"in"
       ,"pkgs.stdenv.mkDerivation {"
       ,"  name = \"my-haskell-env-0\";"
       ,"  buildInputs = [ ghc " ++ intercalate " " (map parens cfgNixOtherDeps) ++ "];" -- todo system build inputs here
       ,"  shellHook = \"eval $(egrep ^export ${ghc}/bin/ghc)\";"
       ,"}"]
  

parens :: [Char] -> [Char]
parens x = "(" ++ x ++ ")"
