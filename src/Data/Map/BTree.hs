{-# LANGUAGE ScopedTypeVariables #-}

module Data.Map.BTree (
    Map,
    empty,
    singleton,
) where

    import           Control.Exception              (assert)
    import           Data.Map.BTree.Internal.Branch
    import           Data.Map.BTree.Internal.Class
    import           Data.Map.BTree.Internal.Leaf
    import           Data.Map.BTree.Internal.Splice
    import           Data.Map.BTree.Internal.Type

    empty :: Map k v 
    empty = Empty

    singleton :: k -> v -> Map k v
    singleton = Singleton

    
    fromAscListUnchecked :: [(k,a)] -> Map k a
    fromAscListUnchecked []         = Empty
    fromAscListUnchecked [ (k, v) ] = Singleton k v
    fromAscListUnchecked xs         =
            case partitionBlocks xs of
                Left ys ->
                    let (k,b) = makeLeaf ys in
                    Block k b
                Right ys ->
                    go (makeLeaf <$> ys)
        where
            -- The following algorithm is O(n): a sketch of the proof.
            -- Going over the whole list to convert to Leafs n * c
            -- for some constant c.  But the result list is n/optSize
            -- Leafs long.  So converting the [ Leaf k v ] to 
            -- [ Block Leaf k v ] costs (n * c / optSize).
            -- The [ Block Leaf k v ] -> [ Block (Block Leaf) k v ]
            -- conversion has a cost of (n * c / optSize^2).
            -- And so on.  The lists get short in a hurry, in other
            -- words.  Then:
            --      (n * c) + (n * c / optSize) + (n * c / optSize^2) + ...
            --      = n * c * (1 + 1/optSize + 1/optSize^2 + ...)
            -- The series 1 + 1/optSize + 1/optSize^2 + ... converges
            -- to optSize/(optSize - 1).  So the cost is:
            --      n * c * optSize / (optSize - 1)
            -- Note that assuming optSize = 12, optSize / (optSize - 1)
            -- is 1.090909... So not only are we O(n), we're a pretty
            -- tight O(n).
            go :: forall c k v . BTree c => [ (k, c k v) ] -> Map k v
            go []   = assert False $ error "Unreachable code reached."
            go [x]  = uncurry Block x
            go ckvs = case partitionBlocks ckvs of
                        Left  zs  -> uncurry Block $ makeBranch zs 
                        Right zzs -> go $ makeBranch <$> zzs

    fromAscList :: Ord k => [(k,v)] -> Map k v
    fromAscList = fromAscListUnchecked . checkArray
        where
            checkArray ((k0, v0) : (kvs@((k1, _) : _))) =
                if (k0 < k1)
                then (k0, v0) : checkArray kvs
                else error "fromAscList: list is not strictly ascending!"
            checkArray kvs = kvs

    lookup :: Ord k => k -> Map k v -> Maybe v
    lookup _ Empty = Nothing
    lookup k (Singleton k1 v1)
        | k == k1   = Just v1
        | otherwise = Nothing
    lookup k (Block k1 block) = doLookup k (k1, block)

