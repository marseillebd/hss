module Hss
  (
    module X
  -- * Shell
  , exe
  , (|>)
  , (&>)
  , (&>>)
  , sponge
  , spongeLines
  -- * Environment
  , getProgName
  , getArgs
  , getEnv
  , getEnvironment
  , stdin
  , stdout
  , stderr
  -- ** Strings
  , here
  -- ** Paths
  , module Hss.Path
  , readFile
  , writeFile
  ) where

import Hss.Preprelude as X
import Hss.String as X

import Hss.String.Convert (bs2lbs, strHs2Os, strOs2Hs)
import Hss.Path


import Data.String.Here.Uninterpolated (here)
import Shh (exe, (|>), Shell)
import System.IO (IOMode(..), withFile, openFile, stdin, stdout, stderr)
import System.Process.Environment.OsString (getArgs, getEnv, getEnvironment)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Shh
import qualified System.Environment

infixl 9 &>
(&>) :: Shh.Shell m => Shh.Proc a -> Path -> m a
p &> fpath = p Shh.&> Shh.Truncate (bs2lbs . intoBytes $ fpath)

infixl 9 &>>
(&>>) :: Shh.Shell m => Shh.Proc a -> Path -> m a
p &>> fpath = p Shh.&> Shh.Append (bs2lbs . intoBytes $ fpath)

-- TODO move to an IO module
readFile :: Path -> IO Bytes
readFile path = openFile (strOs2Hs path) ReadMode >>= BS.hGetContents
writeFile :: IntoOsStr path => path -> Bytes -> IO ()
writeFile path content = withFile (strOs2Hs $ intoPath path) WriteMode $ \fp ->
  BS.hPutStr fp content

instance Shh.ExecArg OsStr where
  asArg s = [bs2lbs . intoBytes $ s]

sponge :: (Functor io, Shell io) => io Bytes
sponge = LBS.toStrict <$> Shh.capture

spongeLines :: (Functor io, Shell io) => io [Bytes]
spongeLines = fmap LBS.toStrict <$> Shh.captureLines

getProgName :: IO OsStr
getProgName = strHs2Os <$> System.Environment.getProgName
