{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module Data.Map.BTree.Internal.Class where

    class BTree t where
        type Child t k v :: *
        getSize :: forall k v . (k, t k v) -> Int
        doLookup :: forall k v . Ord k => k -> (k, t k v) -> Maybe v


