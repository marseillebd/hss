module Main where

import Prelude hiding (lines, writeFile, readFile)
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)

import Hss

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Char8 as BS8


main :: IO ()
main = getArgs >>= \case
  scriptArg : otherArgs -> withTempDir $ \tmpdir -> do
    let scriptPath = toOsPath (encodeUtf8 $ T.pack scriptArg)
        scriptDir = dirname scriptPath
        scriptName = basename scriptPath -- FIXME encode names outside of usual identifiers
    readFile scriptPath >>= \case
      content | "#!" `BS.isPrefixOf` content -> do
        writeFile (tmpdir </> "Main.hs") "\n"
        exe "tail" "-n+2" scriptPath &>> (tmpdir </> "Main.hs")
              | otherwise ->
        exe "cp" scriptPath (tmpdir </> "Main.hs")
    let freshCabal = tmpdir </> scriptName <.> "cabal"
    writeFile freshCabal (encodeUtf8 templateCabal)
    writeFile (tmpdir </> "cabal.project") (encodeUtf8 cabalProject)
    exe "sed" "-i" ("s/SCRIPTNAME/"<>scriptName<>"/") freshCabal
    absExePath <- withCd tmpdir $ do
      exe "cabal" "build"
      relExePaths <- exe "find" "-executable" "-type" "f" "-name" scriptName |> capture
      let relExePath = toOsPath . head $ BS8.lines $ LBS.toStrict relExePaths
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
  location: /home/marseillebd/Documents/programming/hss
  -- branch: main
|]

templateCabal :: Text
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
