
module Data.Map.BTree.Internal.Class where

    class BTree t where
        type Child t k v :: *
        isEmpty' :: forall k v . t k v -> Bool
        lookup' :: forall k v . Ord k => k -> t k v -> Maybe v
        makeRaw :: forall k v . [ (k, Child t k v) ] -> t k v


