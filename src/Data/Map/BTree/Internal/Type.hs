{-# LANGUAGE GADTs               #-}

module Data.Map.BTree.Internal.Type where

    import           Control.DeepSeq
    import           Data.Map.BTree.Internal.Class

    data Map k v where
        Empty :: Map k v
        Singleton :: k -> v -> Map k v
        Block :: BTree t => k -> t k v -> Map k v

    instance NFData2 Map where
        liftRnf2 _  _  Empty           = ()
        liftRnf2 fk fv (Singleton k v) = fk k `seq` fv v `seq` ()
        liftRnf2 fk fv (Block k t)     = fk k `seq` liftRnf2 fk fv t `seq` ()

    instance (NFData k, NFData v) => NFData (Map k v) where
        rnf = rnf2
