{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

{-|
Module          : Data.Map.BTree.Internal.Block
Description     : Definition of a block and associated low-level operations.
Copyright       : (c) Brian Hurt, 2020
Licence         : BSD 3-clause
Maintainer      : bhurt42@gmail.com
Stability       : Internal
Portability     : Safe

IMPORTANT NOTICE: This is an internal module to this package, and not
intended for external use.  It is subject to change without notice,
even in minor releases.  Use at your own risk.

The core data structure used by btree-map is the block.  There are
two types of blocks.  A leaf block contains an array of n values
and an array of n-1 keys- the first (minimum) key is kept in the
next level up, so that any give key is only stored once.  A branch
block contains an array of n child blocks, an array of n-1 keys (the
first key being kept one level up), and an array of n sizes.

We want to split blocks into peices, and then recombine them.  We'd
like to be able to abstract as much of the splitting and recombining
as possible.  Unfortunately, the two different types of block with
two different numbers of arrays (2 for leaf blocks, 3 for branch blocks)
make having any sort of shared abstract structure too hard.

Fortunately, we don't really need to.  We introduce the notion of a splice-
some underlying abstract value (which in practice will be either a leaf
or branch block) with an offset and a count.  A splice represents a part
of a block, then.  Splices are then easy to break apart- take and drop
can be implemented in O(1).

And they are easy to recombine- given a function that converts a splice
into one or the other internal arrays, we can implement a function that
converts a list of splices into a single array.  A leaf block would
call this function twice, once for the keys and once for the values.
A branch block would call it three times.

As this is an internal-only, not for external usage, module, input
validation is done by assert in most cases.  Which means in production
it is not checked.  The behavior of these functions if their inputs
are invalid is undefined.  You were warned.

-}
module Data.Map.BTree.Internal.Splice (
    Splice(..),
    makeSplice,
    takeSplice,
    dropSplice,
    spliceIndex,
    takeSpliceList,
    dropSpliceList,
    splitAtSpliceList,
    balanceSplices,
    spliceListToSmallArray,
    spliceListToKeySmallArray,
    partitionBlocks
) where

    import           Control.Exception                 (assert)
    import           Control.Monad                     (when)
    import           Control.Monad.ST
    import qualified Data.List                         as List
    import           Data.Map.BTree.Internal.Constants
    import           Data.Primitive.SmallArray

    -- | A splice, a sub-part of some underlying block.
    --
    -- A splice is always required to have at least 1 element (i.e.
    -- spliceCount must be at least 1)- we don't deal with empty splices. 
    --
    -- Also, a splice is assumed to be a subset of the underlying block-
    -- specifically, for a given splice s, the indexes (spliceOffset s)
    -- through (spliceOffset s + spliceCount s - 1) must be valid
    -- indexes of all arrays of the block (spliceValue s).  
    data Splice a = Splice {
                        -- | Index of the first element of the splice
                        -- in the underlying value.
                        --
                        -- Must be >= 0.
                        spliceOffset :: Int,

                        -- | Number of elements in the splice.
                        spliceCount :: Int,

                        -- | The underlying value.
                        spliceValue  :: a }

    instance Functor Splice where
        fmap f splice = splice { spliceValue = f (spliceValue splice) }

    -- TODO: Add Applicative, Monad, Foldable, Traversable instances?
    -- Are these useful?  Sanely definable?
    --
    -- Note: Do NOT add Semigroup/Monoid instances!  They will not do
    -- what people expect them to do, and thus would be attractive
    -- nuiscances.

    -- | Create a splice.
    --
    -- The initial offset of the created splice is 0.  We do not (can not)
    -- check that the length given is correct.
    makeSplice ::   Int  -- ^ Initial length
                    -> a -- ^ Splice value
                    -> Splice a
    makeSplice len val =
        assert (len > 0) $ Splice 0 len val

    -- | Sanity check a splice.
    --
    -- Used in asserts
    validSplice :: Splice a -> Bool
    validSplice splice = (spliceOffset splice >= 0)
                            && (spliceCount splice > 0)

    -- | Create a new splice which is the first n elements of a given splice.
    --
    -- Equivalent to take for lists.  Operates in O(1) and is cheap.
    --
    -- The value of n needs to be less than or equal to the length of the
    -- given splice, and greater than 0 (no empty splices).
    takeSplice :: Int -> Splice a -> Splice a
    takeSplice n splice =
        assert (validSplice splice) $
        assert (n > 0) $
        assert (n <= spliceCount splice) $
        splice { spliceCount = n }

    -- | Create a new splice which is everything but the first n elements
    -- of a given splice.
    --
    -- Equivalent to drop for lists.  Operates in O(1) and is cheap.
    --
    -- The value of n needs to be less than the length of the given splice
    -- (no empty splices) and greater than or equal to 0.
    dropSplice :: Int -> Splice a -> Splice a
    dropSplice n splice =
        assert (validSplice splice) $
        assert (n >= 0) $
        assert (n < spliceCount splice) $
        splice {
            spliceOffset =  spliceOffset splice + n,
            spliceCount = spliceCount splice - n }

    -- | Create a new splice list which is the first n elements of
    -- a given splice list.
    --
    -- Equalivalent to take for lists.
    takeSpliceList :: Int -> [ Splice a ] -> [ Splice a ]
    takeSpliceList _ [] = assert False [] -- We should never get here
    takeSpliceList n (x : xs) =
        assert (validSplice x) $
        assert (n > 0) $
        if (n < spliceCount x)
        then [ takeSplice n x ]
        else
            if (n == spliceCount x)
            then [ x ]
            else
                x : takeSpliceList (n - spliceCount x) xs

    dropSpliceList :: Int -> [ Splice a ] -> [ Splice a ]
    dropSpliceList _ [] = assert False [] -- We should never get here
    dropSpliceList n (x : xs) =
        assert (validSplice x) $
        assert (n > 0) $
        if (n < spliceCount x)
        then dropSplice n x  : xs
        else
            if (n == spliceCount x)
            then xs
            else dropSpliceList (n - spliceCount x) xs

    splitAtSpliceList :: Int -> [ Splice a ] -> ([ Splice a ], [ Splice a ])
    splitAtSpliceList _ [] = -- We should never get here
        assert False $
            error "splitAtSpliceList should not be given an empty list."
    splitAtSpliceList n (x : xs) =
        assert (validSplice x) $
        assert (n > 0) $
        if (n < spliceCount x)
        then ([ takeSplice n x ], (dropSplice n x : xs))
        else
            if (n == spliceCount x)
            then ([ x ], xs)
            else
                let (h, t) = splitAtSpliceList (n - spliceCount x) xs in
                ((x : h), t)

    spliceListLength :: [ Splice a ] -> Int
    spliceListLength splices = sum $ spliceCount <$> splices

    -- Get a specific element of a splice, given an index function of the
    -- underlying block.
    --
    -- This function handles the offset.  It's not much, but it's
    -- honest work.
    spliceIndex :: (a -> Int -> b) -> Splice a -> Int -> b
    spliceIndex f splice i =
        assert (validSplice splice) $
        assert (i >= 0) $
        assert (i < spliceCount splice) $
        f (spliceValue splice) (i + spliceOffset splice)


    spliceListToSmallArray :: forall a b .
                                (a -> SmallArray b)
                                -> [ Splice a ]
                                -> SmallArray b
    spliceListToSmallArray f splices =
            assert splicesNotEmpty $
            assert (totalCount > 0) $
            runSmallArray $ do
                arr <- newSmallArray totalCount undefined
                go arr 0 splices
                return arr
        where
            splicesNotEmpty :: Bool
            splicesNotEmpty = case splices of
                                    [] -> False
                                    _  -> True

            totalCount :: Int
            totalCount = spliceListLength splices

            go :: SmallMutableArray s b -> Int -> [ Splice a ] -> ST s ()
            go _   _   []       = return ()
            go arr idx (x : xs) =
                assert (validSplice x) $ do
                let n = spliceCount x
                copySmallArray arr idx (f (spliceValue x)) (spliceOffset x) n
                go arr (idx + n) xs

    spliceListToKeySmallArray :: forall a b .
                                    (a -> (b, SmallArray b))
                                    -> [ Splice a ]
                                    -> (b, SmallArray b)
    spliceListToKeySmallArray f splices = 
            assert splicesNotEmpty $
            assert (totalCount > 0) $
            (k0, ks)
        where
            splicesNotEmpty :: Bool
            splicesNotEmpty = case splices of
                                    [] -> False
                                    _  -> True

            totalCount :: Int
            totalCount = spliceListLength splices

            k0 :: b
            k0 = case splices of
                    []    -> assert False $ error "Unreachable code reached"
                    (splice:_) -> 
                        assert (spliceCount splice > 0) $
                        let (s1, ss) = f (spliceValue splice) in
                        if (spliceOffset splice == 0)
                        then s1
                        else indexSmallArray ss (spliceOffset splice - 1)

            ks :: SmallArray b
            ks = runSmallArray go

            go :: forall s . ST s (SmallMutableArray s b)
            go = do
                arr <- newSmallArray (totalCount - 1) undefined
                case splices of
                    []    -> assert False $ error "Unreachable code reached"
                    (splice:more) -> 
                        assert (spliceCount splice > 0) $
                        if (spliceCount splice > 1)
                        then do
                            let (_, ss) = f (spliceValue splice)
                            copySmallArray arr 0 ss (spliceOffset splice)
                                    (spliceCount splice - 1)
                            go2 arr (spliceCount splice - 1) more
                        else go2 arr 0 more

            go2 :: forall s .
                        SmallMutableArray s b
                        -> Int
                        -> [ Splice a ]
                        -> ST s (SmallMutableArray s b)
            go2 arr _   []            = return arr
            go2 arr off (splice:more) =
                assert (spliceCount splice > 0) $ do
                let (s, ss) = f (spliceValue splice)
                if (spliceOffset splice == 0)
                then do
                    writeSmallArray arr off s
                    when (spliceCount splice > 1) $ do
                        copySmallArray arr (off + 1) ss 0
                                (spliceCount splice - 1)
                else
                    copySmallArray arr off ss (spliceOffset splice - 1)
                        (spliceCount splice)
                go2 arr (off + spliceCount splice) more

    data BalanceBlock = BalanceBlock {
                            extraElems :: Int,
                            baseSize :: Int }

    balanceBlock :: Int -> BalanceBlock
    balanceBlock n = BalanceBlock {
                            extraElems = extraSize,
                            baseSize = base
                        }
        where
            -- quot rounds down- we want to round to nearest.  So we
            -- add an offset which makes quot round the way we want to-
            -- If optSize == 12, then (17 + offset) `quot` optSize == 1,
            -- while (18 + offset) `quot` optSize == 2.
            offset :: Int
            offset = optSize `quot` 2

            nBlocks :: Int
            nBlocks = assert (n > 0) $ (n + offset) `quot` optSize

            -- We want to partition things into extraSize blocks of
            -- (base + 1) elements, and (nBlocks - extraSize)
            -- blocks of base elements.
            base :: Int
            base = n `quot` nBlocks

            extraSize :: Int
            extraSize = n `mod` nBlocks


    balanceSplices :: [ Splice a ] -> [ [ Splice a ] ]
    balanceSplices splices =
            assert (n > 0) $
            foo (extraElems balance) splices
        where

            n :: Int
            n = spliceListLength splices

            balance :: BalanceBlock
            balance = balanceBlock n

            foo :: Int -> [ Splice a ] -> [ [ Splice a ] ]
            foo _ [] = []
            foo 0 xs =
                let (h, t) = splitAtSpliceList (baseSize balance) xs in
                h : foo 0 t
            foo i xs =
                let (h, t) = splitAtSpliceList (baseSize balance + 1) xs in
                h : foo (i-1) t

    partitionBlocks :: [ a ] -> Either [ a ] [ [ a ] ]
    partitionBlocks elems =
            if (atLeast minSize elems)
            then Right $ go elems
            else Left $ elems
        where
            atLeast :: Int -> [ a ] -> Bool
            atLeast 0 _      = True
            atLeast i (_:xs) = atLeast (i-1) xs
            atLeast _ []     = False

            go :: [ a ] -> [ [ a ] ]
            go lst =
                if (atLeast (3 * optSize) lst)
                then
                    let (h, t) = List.splitAt optSize lst in
                    h : go t
                else
                    let balance :: BalanceBlock
                        balance = balanceBlock (length lst)
                    in go2 (baseSize balance) (extraElems balance) lst

            go2 :: Int -> Int -> [ a ] -> [ [ a ] ]
            go2 _  ext [] = assert (ext == 0) []
            go2 sz 0   xs = 
                let (h, t) = List.splitAt sz xs in
                h : go2 sz 0 t
            go2 sz ext xs =
                let (h, t) = List.splitAt (sz + 1) xs in
                h : go2 sz (ext-1) t


