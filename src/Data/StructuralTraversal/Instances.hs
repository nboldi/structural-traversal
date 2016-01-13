{-# LANGUAGE FlexibleInstances
           , UndecidableInstances
           , TemplateHaskell
           #-}
module Data.StructuralTraversal.Instances where

import Data.StructuralTraversal.Class
import Control.Applicative
import Data.Traversable
import Data.StructuralTraversal.TH

deriveStructTrav ''[]
deriveStructTrav ''Maybe
deriveStructTrav ''Either

