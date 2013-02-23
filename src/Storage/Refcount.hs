{-# LANGUAGE NoImplicitPrelude
           #-}
module Storage.Refcount ( Refcount
                        , refInsert
                        , refDelete
                        ) where

import Prelewd

import Storage.Map
import Subset.Num

type Refcount a = Map a (Positive Integer)

refInsert :: Ord a => a -> Refcount a -> Refcount a
refInsert v = insertWith (+) v 1

refDelete :: Ord a => a -> Refcount a -> Maybe (Refcount a)
refDelete = modify $ \v -> toPos $ fromIntegral v - 1
