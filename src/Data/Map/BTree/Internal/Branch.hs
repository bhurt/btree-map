{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module Data.Map.BTree.Internal.Branch (
    Branch,
    makeBranch
) where

    import           Control.DeepSeq
    import           Control.Exception             (assert)
    import           Data.Map.BTree.Internal.Class
    import           Data.Map.BTree.Internal.Util
    import           Data.Primitive.SmallArray

    data Branch c k v = Branch {
        branchKeys :: SmallArray k,
        branchChildren :: SmallArray (c k v),
        branchSizes :: SmallArray Int
    }

    instance NFData2 c => NFData2 (Branch c) where
        liftRnf2 fk fv b =
            forceSmallArray fk (branchKeys b)
            `seq` forceSmallArray rwhnf (branchSizes b)
            `seq` forceSmallArray (liftRnf2 fk fv) (branchChildren b)
            `seq` ()

    instance BTree c => BTree (Branch c) where
        type Child (Branch c) k v = c k v

        getSize (_, branch) =
            assert (checkBranch branch) $
            sum (branchSizes branch)

        doLookup k branch =
                assert (checkBranch (snd branch)) $
                loop 0
            where
                n :: Int
                n = getLength branch

                loop i
                    | i < (n - 1) =
                        let ki1 = getKey branch (i + 1) in
                        if k < ki1
                        then doLookup k (getKey branch i, getChild branch i)
                        else loop (i + 1)
                    | otherwise =
                        doLookup k (getKey branch i, getChild branch i)


    getLength :: (k, Branch c k v) -> Int
    getLength (_, branch) = sizeofSmallArray (branchChildren branch)

    getKey :: (k, Branch c k v) -> Int -> k
    getKey (k, branch) i
        | i == 0 = k
        | otherwise = indexSmallArray (branchKeys branch) (i - 1)

    getChild :: (k, Branch c k v) -> Int -> c k v
    getChild (_, branch) i = indexSmallArray (branchChildren branch) i

        
    checkBranch :: Branch c k v -> Bool
    checkBranch branch = 
            (sizeofSmallArray (branchKeys branch)
                        == sizeofSmallArray (branchChildren branch) - 1)
            && (sizeofSmallArray (branchChildren branch)
                        == sizeofSmallArray (branchSizes branch))


    makeBranch :: BTree c => [ (k, c k v) ] -> (k, Branch c k v)
    makeBranch xs =
            case (fst <$> xs) of
                [] -> assert False $ error "Can not pass an empty list\
                                            \ to makeRaw!"
                (k:ks) -> 
                    let ca = smallArrayFromList (snd <$> xs)
                        sa = smallArrayFromList (getSize <$> xs)
                        ka = smallArrayFromList ks
                    in
                    (k, Branch {    branchKeys = ka,
                                    branchChildren = ca,
                                    branchSizes = sa })

    {-
    spliceFromBranch :: BTree c => (k, Branch c k v)
                                -> Splice (k, Branch c k v)
    spliceFromBranch branch = makeSplice (getSize branch) branch

    branchFromSpliceList :: forall c k v .
                                [ Splice (k, Branch c k v) ]
                                -> (k, Branch c k v)
    branchFromSpliceList splices =
        let ca :: SmallArray (c k v)
            ca = spliceListToSmallArray (branchChildren . snd) splices
            sa :: SmallArray Int
            sa = spliceListToSmallArray (branchSizes . snd) splices
            k :: k
            ka :: SmallArray k
            (k, ka) = spliceListToKeySmallArray (\(x,xs) -> (x, branchKeys xs))
                            splices
        in
        (k, Branch {    branchKeys = ka,
                        branchChildren = ca,
                        branchSizes = sa })

    rebalanceBranches :: BTree c => [ (k, Branch c k v) ]
                                    -> [ (k, Branch c k v) ]
    rebalanceBranches []  = []
    rebalanceBranches [x] = [x]
    rebalanceBranches bs  = branchFromSpliceList <$>
                                balanceSplices (spliceFromBranch <$> bs)

    -}
