{-# LANGUAGE GADTs               #-}

module Data.Map.BTree.Internal.Type where

    import           Data.Map.BTree.Internal.Class

    data Map k v where
        Empty :: Map k v
        Singleton :: k -> v -> Map k v
        Block :: BTree t => k -> t k v -> Map k v


