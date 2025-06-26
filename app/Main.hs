#!/usr/bin/env hss
module Main where

import Hss
import Hss.Process

import qualified Hss.LinkedList as LL


main :: IO ()
main = getArgs >>= \case
  scriptPath : otherArgs -> withTempDir $ \tmpdir -> do
    let scriptDir = dirname scriptPath
        scriptName = basename scriptPath -- FIXME encode names outside of usual identifiers
    runShell $ simple "cp" [scriptPath, tmpdir </> "Main.hs"]
    -- exe "cp" scriptPath (tmpdir </> "Main.hs")
    let freshCabal = tmpdir </> scriptName <.> "cabal"
    writeFile freshCabal (intoBytes templateCabal)
    writeFile (tmpdir </> "cabal.project") (intoBytes cabalProject)
    exe "sed" "-i" ("s/SCRIPTNAME/"<>scriptName<>"/") freshCabal
    absExePath <- withCd tmpdir $ do
      exe "cabal" "build"
      relExePaths <- exe "find" "-executable" "-type" "f" "-name" scriptName |> spongeLines
      let relExePath = intoPath . maybe undefined id . LL.head $ relExePaths
      pure $ tmpdir </> relExePath
    runShell $ do
      let scriptExePath = scriptDir </> "."<>scriptName
      simple "mv" [absExePath, scriptExePath]
      simple scriptExePath otherArgs
  _ -> pure () -- TODO exitFailure

cabalProject :: Text
cabalProject = [here|
packages: .
with-compiler: ghc-9.12.2
allow-newer: shh:base, shh:template-haskell

source-repository-package
  type: git
  location: https://github.com/marseillebd/hss
  branch: main
|]

templateCabal :: Text
templateCabal = [here|
cabal-version:  3.0
name:           SCRIPTNAME
version:        0.0.0.0
build-type:     Simple

executable SCRIPTNAME
  main-is:        Main.hs
  build-depends:  hss
  mixins:
    hss (Hss as Prelude),
    hss,

  default-language: Haskell2010
  default-extensions:
    ExtendedDefaultRules,
    LambdaCase,
    NamedDefaults,
    OverloadedStrings,
    QuasiQuotes,
    TemplateHaskell,
    TypeApplications,

  ghc-options: -Wall -Wno-type-defaults
|]
