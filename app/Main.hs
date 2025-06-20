module Main where

import Hss

import qualified Hss.Bytes as B
import qualified Hss.LinkedList as LL


main :: IO ()
main = getArgs >>= \case
  scriptArg : otherArgs -> withTempDir $ \tmpdir -> do
    let scriptPath = hsStrToPath scriptArg
        scriptDir = dirname scriptPath
        scriptName = basename scriptPath -- FIXME encode names outside of usual identifiers
    readFile scriptPath >>= \case
      content | "#!" `B.isPrefixOf` content -> do
        writeFile (tmpdir </> "Main.hs") "\n"
        exe "tail" "-n+2" scriptPath &>> (tmpdir </> "Main.hs")
              | otherwise ->
        exe "cp" scriptPath (tmpdir </> "Main.hs")
    let freshCabal = tmpdir </> scriptName <.> "cabal"
    writeFile freshCabal (textToBytes templateCabal)
    writeFile (tmpdir </> "cabal.project") (textToBytes cabalProject)
    exe "sed" "-i" ("s/SCRIPTNAME/"<>scriptName<>"/") freshCabal
    absExePath <- withCd tmpdir $ do
      exe "cabal" "build"
      relExePaths <- exe "find" "-executable" "-type" "f" "-name" scriptName |> captureLines
      let relExePath = bytesToPath . maybe undefined id . LL.head $ relExePaths
      pure $ tmpdir </> relExePath
    let scriptExePath = scriptDir </> "."<>scriptName
    exe "mv" absExePath scriptExePath
    exe scriptExePath otherArgs -- TODO apply other args
  _ -> pure () -- TODO exitFailure

cabalProject :: Text
cabalProject = [here|
packages: .

source-repository-package
  type: git
  -- FIXME use a hss release off github
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
    OverloadedStrings,
    TemplateHaskell,
    QuasiQuotes,

  ghc-options: -Wall -Wno-type-defaults
|]
