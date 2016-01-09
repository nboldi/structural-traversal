module Data.StructuralTraversal.Class where

import Control.Applicative
 
class StructuralTraversable t where
  structTraverse :: Applicative f => f () -> f () -> (a -> f b) -> t a -> f (t b)
  
