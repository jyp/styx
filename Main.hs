{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where
import System.Process
import Control.Monad.Identity
import Data.Yaml hiding (Parser)
import Data.Yaml.Config
import qualified Data.Map as M
import Data.Map (Map)
import Data.Aeson.Types (typeMismatch)
import Options.Applicative
import Data.List (intercalate)
import System.Directory (createDirectoryIfMissing, canonicalizePath)
import Prelude hiding (log)

parens :: [Char] -> [Char]
parens x = "(" ++ x ++ ")"

-----------------------------------------
-- OPTIONS

data Command = Configure | Cabal [String] | Clean

withInfo :: Parser a -> String -> ParserInfo a
withInfo opts desc = info (helper <*> opts) $ progDesc desc

parseExec :: Parser Command
parseExec = (\rest -> Cabal (["exec","--"] ++ rest)) <$> some (argument str (metavar "COMMAND"))

parseCommand :: Parser Command
parseCommand = subparser $
    command "configure" (pure Configure `withInfo` "Re-configure the project on the basis of the styx.yaml file") <>
    command "clean"     (pure Clean `withInfo` "Remove all styx working files") <>
    command "build"     (pure (Cabal ["install"]) `withInfo` "(Attempt to) build and install all the packages in the sandbox") <>
    command "repl"      (pure (Cabal ["repl"]) `withInfo` "Start a repl in the nix-shell'ed cabal sandbox") <>
    command "exec"      (parseExec `withInfo` "Exec a command in the nix-shell'ed cabal sandbox")

main :: IO ()
main = run =<< execParser (parseCommand `withInfo` "Wrapper around nix-shell, cabal2nix and cabal")

--------------------------------
-- Configuration 
data Repo = Repo {repoLocation :: String,
                  repoRevision :: Maybe String}
data Config =
  Config {cfgNixpkgsVersion :: Maybe GitVersion
         ,cfgLocalPackages :: Map String Repo -- list of local packages (must be on the local filesystem)
         ,cfgExternalSourceDeps :: Map String Repo -- mapping of package names to locations as understood by cabal2nix
         ,cfgNixHsDeps :: [String]
         -- ^ haskell deps to fetch directly from nix (usually empty for a cabal project, as the cabal file will specifiy deps)
         ,cfgNixOtherDeps :: [String]
         -- ^ Other nix dependencies (non haskell packages)
         ,cfgDefCompil :: String 
         }

data ShellConfig = ShellConfig {}

instance FromJSON Config where
  parseJSON (Object v) = Config <$>
                         v .:? "nixpkgs" <*>
                         v .:? "local-packages" .!= M.empty  <*>
                         v .:? "source-deps" .!= M.empty  <*>
                         v .:? "nix-deps" .!= []  <*>
                         v .:? "non-haskell-deps" .!= [] <*>
                         v .:? "default-compiler" .!= "ghc801"
  parseJSON invalid = typeMismatch "Config" invalid

instance FromJSON Repo where
  parseJSON (Object v) = Repo  <$>
                         v .: "location"  <*> -- location of the repo (in cabal2nix format)
                         v .:? "revision"
  parseJSON invalid = typeMismatch "Location" invalid

data GitVersion = GitVersion {gitCommit :: String, gitSha :: String}
instance FromJSON GitVersion where
  parseJSON (Object v) = GitVersion <$>
                         v .: "commit" <*>
                         v .: "sha256"   -- find here: curl -LO https://nixos.org/channels/nixpkgs-unstable
  parseJSON invalid = typeMismatch "Git version" invalid


-----------------------------------------
-- Program

locToNix :: String -> Repo -> IO ()
locToNix p (Repo {..}) = do
  cmd $ intercalate " " ["cabal2nix",
                          maybe "" ("--revision=" ++) repoRevision,
                          repoLocation, "> .styx/" ++ p ++ ".nix"]

canonicalizeLocalPath :: Repo -> IO Repo
canonicalizeLocalPath (Repo {repoLocation = d,..}) = do
  repoLocation <- canonicalizePath d
  return (Repo {..})

run :: Command -> IO ()
run c = case c of
  Configure -> configure
  Cabal args -> do
    _ <- cmd ("nix-shell .styx/shell.nix --run " ++ show (intercalate " " ("cabal":args)))
    return ()
  Clean -> cmd "rm -rf .styx"

cmd :: String -> IO ()
cmd x = do
  putStrLn x
  callCommand x

log :: String -> IO ()
log msg = putStrLn $ "STYX: " ++ msg

configure :: IO ()
configure = do
  Config{..} <- loadYamlSettings ["styx.yaml"] [] ignoreEnv
  createDirectoryIfMissing False ".styx"

  log "Running cabal2nix for all local and external packages"
  forM_ (M.assocs cfgLocalPackages) $ \(p,r) -> locToNix p =<< (canonicalizeLocalPath r)
  forM_ (M.assocs cfgExternalSourceDeps) (uncurry locToNix)

  log "Initializing a sandbox in .styx"
  cmd "cabal sandbox init --sandbox .styx"
  log "Adding local packages as sources to the sandbox"
  forM_ (M.assocs cfgLocalPackages) $ \(_,Repo {..}) -> do
    cmd ("cabal sandbox add-source " ++ repoLocation)

  log "Creating shell.nix file"
  writeFile ".styx/shell.nix" $ unlines $
    ["{ nixpkgs ? import <nixpkgs> {}, compiler ? " ++ (show cfgDefCompil) ++ " }:"]
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
       ,"ghc = hp.ghcWithPackages (ps: with ps; stdenv.lib.lists.subtractLists"
       , "[" ++ intercalate " " (M.keys cfgLocalPackages) ++ "]" -- Here we remove the packages that we provide locally in the sandbox
       , "(["
       , intercalate " " (M.keys cfgExternalSourceDeps ++ cfgNixHsDeps)
       ,"  ] " ++ concat [" ++ getHaskellDeps ps ./" ++ n ++ ".nix"| n <- M.keys cfgLocalPackages] ++ "));"
       ,"in"
       ,"pkgs.stdenv.mkDerivation {"
       ,"  name = \"my-haskell-env-0\";"
       ,"  buildInputs = [ ghc " ++ intercalate " " (map parens cfgNixOtherDeps) ++ "];" -- todo system build inputs here
       ,"  shellHook = \"eval $(egrep ^export ${ghc}/bin/ghc)\";"
       ,"}"]
