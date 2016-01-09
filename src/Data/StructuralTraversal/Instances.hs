{-# LANGUAGE FlexibleInstances
           , UndecidableInstances #-}
module Data.StructuralTraversal.Instances where

import Data.StructuralTraversal.Class
import Control.Applicative
import Data.Traversable
 
-- instance {-# OVERLAPPABLE #-} Traversable f => SmartTrav f where
  -- smartTrav desc asc f = traverse (smartTrav desc asc f)