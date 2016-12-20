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
                     ,cfgShell :: Maybe ShellConfig}

data ShellConfig = ShellConfig {shellHsPackages :: [String]}

instance FromJSON Config where
  parseJSON (Object v) = Config <$>
                         v .:? "nix-repos" .!= M.empty  <*> -- list of external repos to depend on
                         v .:? "shell"                      -- Just if not building a package
  parseJSON invalid = typeMismatch "Config" invalid

instance FromJSON Repo where
  parseJSON (Object v) = Repo  <$>
                         v .: "location"               -- location of the repo (in cabal2nix format)
  parseJSON invalid = typeMismatch "Location" invalid

instance FromJSON ShellConfig where
  parseJSON (Object v) = ShellConfig  <$>
                         v .:? "extra-haskell-packages" .!= []
                         -- list of (extra) haskell packages to put in the shell env.
  parseJSON invalid = typeMismatch "ShellConfig" invalid

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
    ["{ nixpkgs ? import <nixpkgs> {}, compiler ? " ++ (show defCompil) ++ " }:"
    ,"with (import <nixpkgs> {}).pkgs;"
    ,"let hp = haskell.packages.${compiler}.override{"
    ,"    overrides = self: super: {"
    ] ++
    ["      " ++ n ++ " = self.callPackage ./" ++ n ++ ".nix {};" | (n,_) <- pkgs] ++
    ["      };};"] ++
    case cfgShell of
      Just (ShellConfig {..}) ->
        ["ghc = hp.ghcWithPackages (ps: with ps; ["
        , intercalate " " (map fst pkgs ++ shellHsPackages)
        ,"  ]);"
        ,"in"
        ,"pkgs.stdenv.mkDerivation {"
        ,"  name = \"my-haskell-env-0\";"
        ,"  buildInputs = [ ghc ];"
        ,"  shellHook = \"eval $(egrep ^export ${ghc}/bin/ghc)\";"
        ,"}"]
      Nothing ->
        ["    locpkg = hp.callPackage ./default.nix { };"
        ,"in locpkg.env"
        ]

