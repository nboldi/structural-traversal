module Data.StructuralTraversal.Class where

import Control.Applicative

class StructuralTraversable t where
  -- Bottom-up traversal
  traverseUp :: Applicative f => f () -> f () -> (a -> f b) -> t a -> f (t b)
  
  -- Top-down traversal
  traverseDown :: Applicative f => f () -> f () -> (a -> f b) -> t a -> f (t b)  
