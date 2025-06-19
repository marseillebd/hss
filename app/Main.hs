module Main where

import Hss

import Data.List (isPrefixOf)

import qualified Data.ByteString.Lazy.Char8 as LBS

main :: IO ()
main = getArgs >>= \case
  scriptPath : otherArgs -> withTempDir $ \tmpdir -> do
    let scriptDir = dirname scriptPath
        scriptName = basename scriptPath -- FIXME encode names outside of usual identifiers
    readFile scriptPath >>= \case
      content | "#!" `isPrefixOf` content -> do
        writeFile (tmpdir </> "Main.hs") "\n"
        exe "tail" "-n+2" scriptPath &>> (tmpdir </> "Main.hs")
              | otherwise ->
        exe "cp" scriptPath (tmpdir </> "Main.hs")
    let freshCabal = tmpdir </> scriptName <.> "cabal"
    writeFile freshCabal templateCabal
    writeFile (tmpdir </> "cabal.project") cabalProject
    exe "sed" "-i" ("s/SCRIPTNAME/"<>scriptName<>"/") freshCabal
    absExePath <- withCd tmpdir $ do
      exe "cabal" "build"
      relExePaths <- exe "find" "-executable" "-type" "f" "-name" scriptName |> capture
      let relExePath = head $ LBS.lines relExePaths
      pure $ tmpdir </> convertStringLike relExePath
    let scriptExePath = scriptDir </> "."<>scriptName
    exe "mv" absExePath scriptExePath
    exe scriptExePath otherArgs -- TODO apply other args
  _ -> pure () -- TODO exitFailure

cabalProject :: String
cabalProject = [here|
packages: .

source-repository-package
  type: git
  -- FIXME use a hss release off github
  location: /home/marseillebd/Documents/programming/hss
  -- branch: main
|]

templateCabal :: String
templateCabal = [here|
cabal-version:  3.0
name:           SCRIPTNAME
version:        0.0.0.0
build-type:     Simple

executable SCRIPTNAME
  main-is:          Main.hs
  build-depends:
    -- TODO use a different base library
    base,
    hss,

  default-language: Haskell2010
  default-extensions:
    ExtendedDefaultRules,
    LambdaCase,
    OverloadedStrings,
    TemplateHaskell,
    QuasiQuotes,

  ghc-options: -Wall -Wno-type-defaults
|]
