{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module Data.Map.BTree.Internal.Branch (
    Branch,
    makeBranch
) where

    import           Control.Exception             (assert)
    import           Data.Map.BTree.Internal.Class
    import           Data.Map.BTree.Internal.Splice
    import           Data.Primitive.SmallArray

    data Branch c k v = Branch {
        branchKeys :: SmallArray k,
        branchChildren :: SmallArray (c k v),
        branchSizes :: SmallArray Int
    }

    instance BTree c => BTree (Branch c) where
        type Child (Branch c) k v = c k v

        getSize (_, branch) =
            assert (sizeofSmallArray (branchKeys branch)
                        == sizeofSmallArray (branchChildren branch) - 1) $
            assert (sizeofSmallArray (branchChildren branch)
                        == sizeofSmallArray (branchSizes branch)) $
            sum (branchSizes branch)

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

