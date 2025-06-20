{-# LANGUAGE DefaultSignatures #-}

module Hss.String
  ( IsString(..)
  , StringLike(..)
  , Text
  , ShortText
  , ByteString
  , ShortByteString
  , convStr
  , toOsPath
  , fromOsPath
  , lines
  ) where

import Prelude (fromIntegral, Num(..), String, Int, undefined, Eq(..))
import Hss.String.Internal

import Hss.Bool
import Hss.Control
import Hss.Data
import Hss.Function

import System.IO (utf8)
import System.OsPath (OsPath, encodeWith, decodeWith)
import Control.Exception (throw)
import System.OsPath.Encoding(utf16le_b)
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Short (ShortByteString)
import Data.Char (Char)
import Data.Maybe (fromJust)
import Data.String (IsString(..))
import Data.Text.Lazy (Text)
import Data.Text.Short (ShortText)
import System.IO (Handle)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.ByteString.Short as SBS
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LT
import qualified Data.Text.Lazy.IO as LT
import qualified Data.Text.Short as ST

class IsString str => StringLike str where
  -- TODO many of these could move into a ListLike class; likewise a TsilLike class
  ------ basic intro/elim forms ------
  empty :: str
  default empty :: Monoid str => str
  empty = mempty
  append :: str -> str -> str
  default append :: Monoid str => str -> str -> str
  append = mappend
  singleton :: Char -> str
  ------ conversions ------
  fromBytes :: ByteString -> str
  toBytes :: str -> ByteString
  fromText :: Text -> str
  toText :: str -> Text
  -- TODO toText? posibly rename to/fromText as en/decodeUtf8
  -- TODO cons, uncons, snoc, unsnoc
  -- TODO append, concat
  -- TODO null

  null :: str -> Bool
  null str = length str == 0
  length :: str -> Int
  elemIndex :: Char -> str -> Maybe Int
  elemIndex c = findIndex (==c)
  findIndex :: (Char -> Bool) -> str -> Maybe Int
  -- TODO map, reverse, intersperse, intercalate
  -- TODO fold{l,r}{,1}{,'}
  -- TODO concat, concatMap, any, all, maximum, minimum
  -- TODO scans?
  -- TODO mapAccum{L,R}?
  -- TODO replicate, unfoldr, unfoldrN
  -- TODO {take,drop}{,While}{,End}, splitAt
  take :: Int -> str -> str
  drop :: Int -> str -> str
  -- TODO split, dropSpace{,End}
  -- TODO {span,break}{,End}
  -- TODO split{,With}
  -- TODO group{,By}?
  -- TODO strip{Pre,Suf}fix, is{Pre,Suf}fixOf
  isPrefixOf :: str -> str -> Bool
  -- TODO isInfixOf?
  ------ IO ------
  hGetContents :: Handle -> IO str
  hPutStr :: Handle -> str -> IO ()

-- TODO head, tail, last, init
-- TODO chomp
-- TODO lines, words, unlines, unwords

-- TODO so, so many more

convStr :: (StringLike a, StringLike b) => a -> b
convStr = fromBytes . toBytes

toHsStr :: StringLike str => str -> String
toHsStr = LT.unpack . toText

fromHsStr :: StringLike str => String -> str
fromHsStr = fromText . LT.pack

lines :: StringLike str => str -> [str]
lines str
  | null str = []
  | True = case '\n' `elemIndex` str of
      Nothing -> [str]
      Just n  -> take n str : lines (drop (n+1) str)

instance StringLike ShortByteString where
  singleton = SBS.singleton . c2w
  fromBytes = SBS.toShort . LBS.toStrict
  toBytes = LBS.fromStrict . SBS.fromShort
  fromText = SBS.toShort . LBS.toStrict . LT.encodeUtf8
  toText = LT.decodeUtf8 . LBS.fromStrict . SBS.fromShort
  length = SBS.length
  elemIndex c = SBS.elemIndex (c2w c)
  findIndex f = SBS.findIndex (f . w2c)
  take = SBS.take
  drop = SBS.drop
  isPrefixOf = SBS.isPrefixOf
  hGetContents fp = SBS.toShort <$> BS.hGetContents fp
  hPutStr fp = BS.hPut fp . SBS.fromShort

instance StringLike ByteString where
  singleton = LBS.singleton
  fromBytes = id
  toBytes = id
  fromText = LT.encodeUtf8
  toText = LT.decodeUtf8
  length = fromIntegral . LBS.length
  elemIndex = (fmap . fmap) fromIntegral . LBS.elemIndex
  findIndex = (fmap . fmap) fromIntegral . LBS.findIndex
  take n = LBS.take (fromIntegral n)
  drop n = LBS.drop (fromIntegral n)
  isPrefixOf = LBS.isPrefixOf
  hGetContents fp = LBS.hGetContents fp
  hPutStr = LBS.hPut

instance StringLike ShortText where
  singleton = ST.singleton
  fromBytes = fromJust . ST.fromByteString . LBS.toStrict
  toBytes = LBS.fromStrict . ST.toByteString
  fromText = ST.fromText . LT.toStrict
  toText = LT.fromStrict . ST.toText
  length = ST.length
  findIndex = ST.findIndex
  take = ST.take
  drop = ST.drop
  isPrefixOf = ST.isPrefixOf
  hGetContents fp = ST.fromText <$> T.hGetContents fp
  hPutStr fp = T.hPutStr fp . ST.toText

instance StringLike Text where
  singleton = LT.singleton
  fromBytes = LT.decodeUtf8
  toBytes = LT.encodeUtf8
  fromText = id
  toText = id
  length = fromIntegral . LT.length
  findIndex = undefined -- TODO
  take n = LT.take (fromIntegral n)
  drop n = LT.drop (fromIntegral n)
  isPrefixOf = LT.isPrefixOf
  hGetContents fp = LT.hGetContents fp
  hPutStr = LT.hPutStr
