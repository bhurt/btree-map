{-|
Module          : Data.Map.BTree.Internal.Constants
Description     : Definition of global constants for the whole library.
Copyright       : (c) Brian Hurt, 2020
Licence         : BSD 3-clause
Maintainer      : bhurt42@gmail.com
Stability       : Internal
Portability     : Safe

IMPORTANT NOTICE: This is an internal module to this package, and not
intended for external use.  It is subject to change without notice,
even in minor releases.  Use at your own risk.

These constants are factored out into their own module to make them
easy to change.  The current values are set to values I think are
reasonable.  I may in the future do some sort of experimentation as
to what the optimal values are.

-}
module Data.Map.BTree.Internal.Constants where

    import           Control.Exception         (assert)

    -- Implementors note: if you make these values significantly larger,
    -- you probably want to change the usage of SmallArray in
    -- Data.Map.BTree.Internal.Block to just Array.  See the implementor's
    -- note in that module for more information.
    
    -- | Minimum size of a block. 
    --
    -- This should be at least 4, currently it defaults to 8.
    minSize :: Int
    minSize = 8

    -- | Maximum size of a block.
    --
    -- This is defined to be 2 * minSize
    maxSize :: Int
    maxSize = assert (minSize > 4) $ 2 * minSize

    -- | The optimal size of a block.
    --
    -- When we're constructing or rebalancing blocks, we prefer to
    -- make blocks of this size, or close to it.  Currently defined
    -- as the average of minSize and maxSize.
    optSize :: Int
    optSize = 
        assert (maxSize > minSize) $
        let r = (minSize + maxSize + 1) `quot` 2 in
        assert ((r >= minSize) && (r <= maxSize)) r

