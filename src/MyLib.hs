{-# LANGUAGE OverloadedStrings #-}

module MyLib
  ( someFunc
  , module BS
  ) where

import Data.ByteString as BS

someFunc :: ByteString
someFunc = "someFunc"
