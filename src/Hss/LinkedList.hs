module Hss.LinkedList
  ( LinkedList
  , head
  ) where

import Prelude (Maybe(..))

type LinkedList a = [a]

head :: LinkedList a -> Maybe a
head [] = Nothing
head (x:_) = Just x
