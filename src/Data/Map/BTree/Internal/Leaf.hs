{-# LANGUAGE TypeFamilies #-}

module Data.Map.BTree.Internal.Leaf (
    Leaf,
    makeLeaf
) where

    import           Control.Exception             (assert)
    import           Data.Map.BTree.Internal.Class
    import           Data.Primitive.SmallArray

    data Leaf k v = Leaf {
        leafKeys :: SmallArray k,
        leafVals :: SmallArray v
    }

    makeLeaf :: [ (k, v) ] -> (k, Leaf k v)
    makeLeaf xs =
        case (fst <$> xs) of
            [] -> assert False $ error "Can not pass an empty list\
                                        \ to makeRaw!"
            (k:ks) -> 
                let va = smallArrayFromList (snd <$> xs)
                    ka = smallArrayFromList ks
                in
                (k, Leaf {  leafKeys = ka,
                            leafVals = va })

    instance BTree Leaf where
        type Child Leaf k v = v

        getSize (_, leaf) =
            let n = sizeofSmallArray (leafVals leaf) in
            assert (sizeofSmallArray (leafKeys leaf) == n - 1) n


