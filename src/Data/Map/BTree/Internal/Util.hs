
module Data.Map.BTree.Internal.Util (
    forceSmallArray
) where

    import           Data.Primitive.SmallArray

    forceSmallArray :: (a -> ()) -> SmallArray a -> ()
    forceSmallArray f = foldr (\a r -> f a `seq` r) ()

