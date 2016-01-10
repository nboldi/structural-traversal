{-# LANGUAGE FlexibleInstances
           , UndecidableInstances
           , TemplateHaskell
           #-}
module Data.StructuralTraversal.Instances where

import Data.StructuralTraversal.Class
import Control.Applicative
import Data.Traversable
import Data.StructuralTraversal.TH

-- instance {-# OVERLAPPABLE #-} Traversable f => SmartTrav f where
  -- smartTrav desc asc f = traverse (smartTrav desc asc f)

deriveStructTrav ''[]
deriveStructTrav ''Maybe
deriveStructTrav ''Either

