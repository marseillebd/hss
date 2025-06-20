module Hss.String.Internal
  ( c2w, w2c
  ) where

import Prelude ((.), fromIntegral)
import Data.Char (Char, ord, chr)
import Data.Word (Word8)

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
