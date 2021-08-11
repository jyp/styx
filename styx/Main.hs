{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where
import System.Process
import Control.Monad.Identity
#if MIN_VERSION_optparse_applicative(0,13,0)
import Data.Monoid ((<>))
#endif
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

data Command = Configure | Cabal [String] | Clean | Command :<> Command

withInfo :: Parser a -> String -> ParserInfo a
withInfo opts desc = info (helper <*> opts) $ progDesc desc

parseExec :: Parser Command
parseExec = (\rest -> Cabal (["v2-exec","--"] ++ rest)) <$> some (argument str (metavar "COMMAND"))

parseRepl :: Parser Command
parseRepl = (\target -> Cabal (["v2-repl"] ++ maybe [] (:[]) target)) <$> optional (argument str (metavar "TARGET"))

parseCabal :: Parser Command
parseCabal = Cabal <$> some (argument str (metavar "COMMAND"))

parseCommand :: Parser Command
parseCommand = subparser $
    command "configure" (pure Configure `withInfo` "Re-configure the project on the basis of the styx.yaml file") <>
    command "clean"     (pure Clean `withInfo` "Remove all styx working files") <>
    command "build"     (pure (Cabal ["v2-build","all"]) `withInfo` "build all the packages") <>
    command "repl"      (parseRepl `withInfo` "Start a repl in the nix-shell'ed 1st component of the cabal project") <>
    command "exec"      (parseExec `withInfo` "Exec a command in the nix-shell'ed cabal project") <>
    command "cabal"     (parseCabal `withInfo` "Execute an arbitrary cabal command in the nix-shell")

main :: IO ()
main = run =<< execParser (parseCommand `withInfo` "Wrapper around nix-shell, cabal2nix and cabal")

--------------------------------
-- Configuration
data Repo = Repo {repoLocation :: String,
                  repoRevision :: Maybe String,
                  repoCabal2NixFlags :: Maybe String}
data Config =
  Config {cfgNixpkgsVersion :: Maybe SourceVersion
         ,cfgLocalPackages :: Map String Repo -- list of local packages (must be on the local filesystem)
         ,cfgExternalSourceDeps :: Map String Repo -- mapping of package names to locations as understood by cabal2nix
         ,cfgNixHsDeps :: [String]
         -- ^ haskell deps to fetch directly from nix (usually empty for a cabal project, as the cabal file will specifiy deps)
         ,cfgNixOtherDeps :: [String]
         -- ^ Other nix dependencies (non haskell packages)
         ,cfgDefCompil :: Maybe String
         }

data ShellConfig = ShellConfig {}

instance FromJSON Config where
  parseJSON (Object v) = Config <$>
                         v .:? "nixpkgs" <*>
                         v .:? "local-packages" .!= M.empty  <*>
                         v .:? "source-deps" .!= M.empty  <*>
                         v .:? "nix-deps" .!= []  <*>
                         v .:? "non-haskell-deps" .!= [] <*>
                         v .:? "default-compiler"
  parseJSON invalid = typeMismatch "Config" invalid

instance FromJSON Repo where
  parseJSON (Object v) = Repo  <$>
                         v .: "location"  <*> -- location of the repo (in cabal2nix format)
                         v .:? "revision" <*>
                         v .:? "cabal2nix"

  parseJSON invalid = typeMismatch "Location" invalid

data SourceVersion = GitVersion {gitOwner :: String, gitCommit :: String, gitSha :: String}
                   | TarballVersion {tarballURL :: String}
instance FromJSON SourceVersion where
  parseJSON (Object v) = (GitVersion <$>
                         v .:? "owner" .!= "NixOS" <*>
                         v .: "commit" <*>
                         v .: "sha256")
                       <|> (TarballVersion <$> v .: "url")
  parseJSON invalid = typeMismatch "Git version" invalid


-----------------------------------------
-- Program

locToNix :: String -> Repo -> IO ()
locToNix p (Repo {..}) = do
  cmd $ intercalate " " ["cabal2nix",
                          maybe "" ("--revision=" ++) repoRevision,
                          maybe "" id repoCabal2NixFlags,
                          repoLocation, "> .styx/" ++ p ++ ".nix"]

canonicalizeLocalPath :: Repo -> IO Repo
canonicalizeLocalPath (Repo {repoLocation = d,..}) = do
  repoLocation <- canonicalizePath d
  return (Repo {..})

run :: Command -> IO ()
run c = case c of
  a :<> b -> run a >> run b
  Configure -> configure
  Cabal args -> do
    _ <- cmd ("nix-shell .styx/shell.nix --pure --run " ++ show (intercalate " " ("cabal":args)))
    return ()
  Clean -> cmd "rm -rf .styx"

cmd :: String -> IO ()
cmd x = do
  putStrLn x
  callCommand x

log :: String -> IO ()
log msg = putStrLn $ "Styx: " ++ msg

configure :: IO ()
configure = do
  Config{..} <- loadYamlSettings ["styx.yaml"] [] ignoreEnv
  createDirectoryIfMissing False ".styx"

  log "Initializing cabal.project"
  writeFile "cabal.project" $ unlines $ ("packages:" : ["  " ++ repoLocation ++ "/" ++ projectName ++ ".cabal"
                                                       | (projectName,Repo {..}) <- M.assocs cfgLocalPackages ] )

  log "Running cabal2nix for all local and external packages"
  forM_ (M.assocs cfgLocalPackages) $ \(p,r) -> locToNix p =<< (canonicalizeLocalPath r)
  forM_ (M.assocs cfgExternalSourceDeps) (uncurry locToNix)


  log "Creating shell.nix file"
  writeFile ".styx/shell.nix" $ unlines $
    ["{ nixpkgs ? import <nixpkgs> {}"
    , maybe "" ((", compiler ? " ++) . show) cfgDefCompil
    ," }:"]
    ++ case cfgNixpkgsVersion of
      Nothing -> ["let nixpkgs' = nixpkgs;"]
      Just source -> ["let nixpkgs_source ="] ++ case source of
        GitVersion {..} -> ["  nixpkgs.fetchFromGitHub {"
                           ,"      owner = " ++ show gitOwner ++ ";"
                           ,"      repo = \"nixpkgs\";"
                           ,"      rev = " ++ show gitCommit ++ ";"
                           ,"      sha256 = " ++ show gitSha ++ ";"
                           ,"    };"
                           ]
        TarballVersion {..} -> ["fetchTarball " ++ show tarballURL ++ ";"]
       ++ ["  nixpkgs' = (import nixpkgs_source){};"]
    ++ ["in with nixpkgs'.pkgs;"
       ,"let hp = " ++ maybe "haskellPackages" (const "haskell.packages.${compiler}") cfgDefCompil ++ ".override{"
       ,"    overrides = self: super: {"
       ]
    ++ ["      " ++ n ++ " = self.callPackage ./" ++ n ++ ".nix {};"
       | n <- (M.keys cfgExternalSourceDeps ++ M.keys cfgLocalPackages)]
    ++ ["      };};"
       ,"     getHaskellDeps = ps: path:"
       ,"        let f = import path;"
       ,"            gatherDeps = { " ++ concat [d ++ " ? [], " | d <- depKinds] ++ "...}:"
       ,"               " ++ intercalate " ++ " depKinds ++ ";"
       ,"            x = f (builtins.intersectAttrs (builtins.functionArgs f)"
       ,"                                               (ps // ",
        "                                                nixpkgs'.pkgs) # can also depend on non-haskell packages",
        "                   // {lib = lib; mkDerivation = gatherDeps;});"
       ,"        in x;"
       ,"ghc = hp.ghcWithPackages (ps: with ps; lib.lists.subtractLists"
       , "[" ++ intercalate " " (M.keys cfgLocalPackages) ++ "]" -- Here we remove the packages that we provide locally in the sandbox
       , "([ cabal-install "
       , intercalate " " (M.keys cfgExternalSourceDeps ++ cfgNixHsDeps)
       ,"  ] " ++ concat [" ++ getHaskellDeps ps ./" ++ n ++ ".nix"| n <- M.keys cfgLocalPackages] ++ "));"
       ,"in"
       ,"pkgs.stdenv.mkDerivation {"
       ,"  name = \"my-haskell-env-0\";"
       ,"  buildInputs = [ glibcLocales ghc " ++ intercalate " " (map parens cfgNixOtherDeps) ++ "];" -- todo system build inputs here
       ,"  shellHook = ''"
       ," export LANG=en_US.UTF-8"
       ," eval $(egrep ^export ${ghc}/bin/ghc)"
       ,"'';"
       ,"}"]
  run (Cabal ["v2-configure"]) -- this will fail unless the sandbox dependencies are built first.

depKinds :: [String]
depKinds = ["buildDepends", "libraryHaskellDepends", "executableHaskellDepends", "libraryToolDepends", "executableToolDepends"]

-- Local Variables:
-- dante-methods: (impure-nix)
-- End:
