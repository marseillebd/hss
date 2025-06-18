module Main where

import Control.Exception (try, bracket, ioError)
import Data.Word (Word32)
import System.Environment (getProgName, getArgs)
import System.FilePath ((</>), (<.>))
import System.IO.Error (isAlreadyExistsError)
import System.Random (randomIO)
import Shh (exe, (|>), (&>), capture)
import Data.Text.Lazy.Encoding (decodeUtf8)
import Data.String (fromString)
import Data.List (isPrefixOf)
import Data.String.Here.Uninterpolated (here)

import qualified Shh
import qualified System.FilePath as Path
import qualified System.Directory as Dir
import qualified Data.Text.Lazy as LT
import qualified Data.ByteString.Lazy as LBS

main :: IO ()
main = getArgs >>= \case
  scriptPath : otherArgs -> withTmpDir $ \tmpdir -> do
    let scriptDir = Path.takeDirectory scriptPath
        scriptName = Path.takeBaseName scriptPath -- FIXME encode names outside of usual identifiers
    readFile scriptPath >>= \case
      content | "#!" `isPrefixOf` content -> do
        writeFile (tmpdir </> "Main.hs") "\n"
        exe "tail" "-n+2" scriptPath &> (Shh.Append . fromString $ tmpdir </> "Main.hs")
              | otherwise ->
        exe "cp" scriptPath (tmpdir </> "Main.hs")
    let freshCabal = tmpdir </> scriptName <.> "cabal"
    writeFile freshCabal templateCabal
    exe "sed" "-i" ("s/SCRIPTNAME/"<>scriptName<>"/") freshCabal
    absExePath <- Dir.withCurrentDirectory tmpdir $ do
      exe "cabal" "build"
      relExePath <- exe "find" "-executable" "-type" "f" "-name" scriptName |> capture
      pure $ tmpdir </> (LT.unpack . LT.takeWhile (/='\n') . decodeUtf8 $ relExePath)
    let scriptExePath = scriptDir </> "."<>scriptName
    exe "mv" absExePath scriptExePath
    exe scriptExePath otherArgs -- TODO apply other args
  _ -> pure () -- TODO exitFailure

withTmpDir :: (FilePath -> IO a) -> IO a
withTmpDir = bracket mkDir rmDir
  where
  mkDir :: IO FilePath
  mkDir = Dir.getTemporaryDirectory >>= loop
  loop :: FilePath -> IO FilePath
  loop parentDir = do
    nonce <- randomIO :: IO Word32
    let dirName = parentDir </> "hss" <.> show nonce
    try (Dir.createDirectory dirName) >>= \case
      Left exn | isAlreadyExistsError exn -> loop parentDir
               | otherwise -> ioError exn
      Right () -> pure dirName
  rmDir = Dir.removePathForcibly

templateCabal :: String
templateCabal = [here|
cabal-version:      3.0
name:               SCRIPTNAME
version:            0.1.0.0
build-type:         Simple

executable SCRIPTNAME
    main-is:          Main.hs
    build-depends:
        base,
        directory,
        filepath,
        -- hss,
        random,
        shh,

    default-language: Haskell2010
    default-extensions:
      ExtendedDefaultRules,
      LambdaCase,
      OverloadedStrings,
      TemplateHaskell,
      QuasiQuotes,

    ghc-options: -Wall
|]
