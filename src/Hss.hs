module Hss
  (
  -- * Shell
    exe
  , (|>)
  , (&>)
  , (&>>)
  , capture
  , captureLines
  -- * Environment
  , getProgName
  , getArgs
  , stdin
  , stdout
  , stderr
  -- * Basic Data Types
  , seq
  -- ** Booleans
  , Bool(..)
  , (&&), (||), not
  , otherwise
  -- ** Strings
  , Char
  , String
  , IsString(..)
  , here
  , ByteString, Text
  -- ** String Conversions
  , hsStrToPath
  , bytesToPath
  , pathToBytes
  , textToBytes
  -- ** Paths
  , module Hss.Path
  , readFile
  , writeFile
  -- ** Functions
  , ($), ($!), (&)
  , id, const, (.)
  , curry, uncurry
  -- ** Numeric
  , Int, Integer
  , Num(..), subtract, Real(..), Integral(..), fromIntegral
  , Float, Double
  , Fractional(..), Floating(..), RealFrac(..), RealFloat(..), realToFrac
  -- * Composite Data Types
  , Maybe(..), maybe
  , Either(..)
  , fst, snd
  -- * Typeclasses
  -- ** Basic
  , Eq(..), Ord(..), Ordering(..)
  , Enum(..), Bounded(..)
  , Semigroup(..), Monoid(..)
  -- ** Categorical
  , Functor(..)
  , Applicative(..)
  , Monad(..), (=<<)
  -- * Composition
  -- * Effects
  , IO
  -- * Prelude
  , undefined
  ) where

import Prelude hiding (readFile, writeFile, String, undefined)
import qualified Prelude

-- import System.IO (withFile, IOMode(..), openFile)

-- import Hss.Applicative
-- import Hss.Base
-- import Hss.Bool
-- import Hss.Data
-- import Hss.Function
-- import Hss.Functor
-- import Hss.IO
-- import Hss.Monad
import Hss.Path
-- import Hss.String

import Data.Text.Encoding (encodeUtf8)
import Control.Exception (throw)
import Data.ByteString (ByteString)
import Data.Function ((&))
import Data.String.Here.Uninterpolated (here)
import Data.String (IsString(..))
import Shh (exe, (|>), Shell)
import System.Environment (getProgName, getArgs)
import System.IO (IOMode(..), withFile, openFile, stdin, stdout, stderr)
import System.IO (utf8)
import System.OsPath (encodeWith, decodeWith)
import System.OsPath.Encoding(utf16le_b)
import Data.Text (Text)

import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString as BS
import qualified Shh
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

type String = [Char]
{-# DEPRECATED String "use Text or any of the other un-slow string types" #-}

infixl 9 &>
(&>) :: Shh.Shell m => Shh.Proc a -> OsPath -> m a
p &> fpath = p Shh.&> Shh.Truncate (fromString . unsafeDecodeUtf $ fpath)

infixl 9 &>>
(&>>) :: Shh.Shell m => Shh.Proc a -> OsPath -> m a
p &>> fpath = p Shh.&> Shh.Append (fromString . unsafeDecodeUtf $ fpath)

-- TODO move to an IO module
readFile :: OsPath -> IO ByteString
readFile path = openFile (unsafeDecodeUtf path) ReadMode >>= BS.hGetContents
writeFile :: OsPath -> ByteString -> IO ()
writeFile path content = withFile (unsafeDecodeUtf path) WriteMode $ \fp ->
  BS.hPutStr fp content

bytesToPath :: ByteString -> OsPath
bytesToPath str = case encodeWith utf8 utf16le_b (T.unpack . T.decodeUtf8 $ str) of
  Right ok -> ok
  Left exn -> throw exn

pathToBytes :: OsPath -> ByteString
pathToBytes str = case decodeWith utf8 utf16le_b str of
  Right ok -> T.encodeUtf8 . T.pack $ ok
  Left exn -> throw exn

instance Shh.ExecArg OsString where
  asArg s = [fromString $ unsafeDecodeUtf s]

hsStrToPath :: String -> OsPath
hsStrToPath = fromString

textToBytes :: Text -> ByteString
textToBytes = encodeUtf8

capture :: (Functor io, Shell io) => io ByteString
capture = LBS.toStrict <$> Shh.capture

captureLines :: (Functor io, Shell io) => io [ByteString]
captureLines = fmap LBS.toStrict <$> Shh.captureLines

undefined :: a
undefined = Prelude.undefined
{-# WARNING undefined "undefined is for temp use only; implement missing functionality, report errors, or panic" #-}
