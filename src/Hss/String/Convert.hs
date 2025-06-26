module Hss.String.Convert
  ( IntoBytes(..)
  , IntoText(..)
  , IntoOsStr(..)
  -- , strHs2B
  , strOs2Hs, strHs2Os
  , bs2lbs
  ) where

import Hss.Preprelude
import Hss.String.Types

import Control.Exception (throw)
import Data.Word (Word8)
import System.IO (utf8)
import System.OsPath (encodeWith, decodeWith, unsafeEncodeUtf)
import System.OsPath.Encoding(utf16le_b)

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

-----------------------------
------ Byte Strings ------
-----------------------------

class IntoBytes str where
  intoBytes :: str -> Bytes

instance IntoBytes Bytes where
  intoBytes = id

instance IntoBytes Text where
  intoBytes = T.encodeUtf8

instance IntoBytes OsStr where
  intoBytes = T.encodeUtf8 . T.pack . strOs2Hs

-----------------------------
------ Text Strings ------
-----------------------------

class IntoText str where
  intoText :: str -> Text

instance IntoText Bytes where
  intoText = T.decodeUtf8

instance IntoText Text where
  intoText = id

instance IntoText OsStr where
  intoText = T.pack . strOs2Hs

-----------------------------
------ Os Strings ------
-----------------------------

class IntoOsStr str where
  intoOsStr :: str -> OsStr
  intoPath :: str -> Path
  intoPath = intoOsStr
default IntoOsStr (OsStr)

instance IntoOsStr Path where
  intoOsStr = id

instance IntoOsStr Bytes where intoOsStr = strB2Os
strB2Os :: Bytes -> OsStr
strB2Os str = case encodeWith utf8 utf16le_b (T.unpack . T.decodeUtf8 $ str) of
  Right ok -> ok
  Left exn -> throw exn

-----------------------------
------ Haskell Strings ------
-----------------------------

strHs2B :: HsString -> Bytes
strHs2B = intoBytes . T.pack

strOs2Hs :: OsStr -> HsString
strOs2Hs str = case decodeWith utf8 utf16le_b str of
  Right ok -> ok
  Left exn -> throw exn

strHs2Os :: HsString -> OsStr
strHs2Os = unsafeEncodeUtf

-----------------------------
------ Lazy and Strict ------
-----------------------------

bs2lbs :: Bytes -> LBS.ByteString
bs2lbs = LBS.fromStrict

-----------------------------
-------- Characters ---------
-----------------------------

-- | Conversion between 'Word8' and 'Char'. Should compile to a no-op.
--
-- Taken from bytestring.
-- TODO use unsafeChr from GHC.Base, perhaps
w2c :: Word8 -> Char
w2c = chr . fromIntegral
{-# INLINE w2c #-}

-- | Unsafe conversion between 'Char' and 'Word8'. This is a no-op and
-- silently truncates to 8 bits Chars > '\255'. It is provided as
-- convenience for ByteString construction.
--
-- Taken from bytestring.
c2w :: Char -> Word8
c2w = fromIntegral . ord
{-# INLINE c2w #-}

