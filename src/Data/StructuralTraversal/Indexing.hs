{-# LANGUAGE LambdaCase #-}
module Data.StructuralTraversal.Indexing where

import Data.StructuralTraversal.Class
import Control.Monad.State
import Control.Applicative

indexedTraverse :: StructuralTraversable t => (a -> [Int] -> b) -> t a -> t b
indexedTraverse f
  = flip evalState []
      . structTraverse ( modify (0:) )
                       ( modify tail ) 
                       ( \a -> f a <$> get <* modify ( \case s:st -> (s+1):st
                                                             []   -> [] ))
                        
  