{-# LANGUAGE TypeFamilies #-}

module Data.Map.BTree.Internal.Leaf (
    Leaf,
    makeLeaf
) where

    import           Control.DeepSeq
    import           Control.Exception             (assert)
    import           Data.Map.BTree.Internal.Class
    import           Data.Map.BTree.Internal.Util
    import           Data.Primitive.SmallArray

    data Leaf k v = Leaf {
        leafKeys :: SmallArray k,
        leafVals :: SmallArray v
    }

    instance NFData2 Leaf where
        liftRnf2 fk fv b =
            forceSmallArray fk (leafKeys b)
            `seq` forceSmallArray fv (leafVals b)
            `seq` ()

    instance BTree Leaf where
        type Child Leaf k v = v

        getSize (_, leaf) =
            let n = sizeofSmallArray (leafVals leaf) in
            assert (sizeofSmallArray (leafKeys leaf) == n - 1) n

        doLookup k kleaf = loop 0
            where
                n :: Int
                n = getSize kleaf

                loop i
                    | i < n     =
                        let ki = getKey kleaf i in
                        case compare k ki of
                            LT -> loop (i + 1)
                            EQ -> Just (getVal kleaf i)
                            GT -> Nothing
                    | otherwise = Nothing


    getKey :: (k, Leaf k v) -> Int -> k
    getKey (k, leaf) i
        | i == 0    = k
        | otherwise = indexSmallArray  (leafKeys leaf) (i - 1)

    getVal :: (k, Leaf k v) -> Int -> v
    getVal (_, leaf) i = indexSmallArray (leafVals leaf) i

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

