{-# LANGUAGE StandaloneDeriving, TemplateHaskell, FlexibleContexts, DeriveTraversable #-}

import Data.StructuralTraversal.Class
import Data.StructuralTraversal.Instances
import Data.StructuralTraversal.TH
import Data.StructuralTraversal.Indexing
import Data.Traversable
import Control.Applicative
import Test.HUnit hiding (test)

data Name a = Name String
     deriving (Show, Eq)

data Lit a = IntLit Integer
     deriving (Show, Eq)

data Expr a = LitExpr (Lit a) 
            | Variable (Name a)
            | Neg (Ann Expr a)
            | Plus (Ann Expr a) (Ann Expr a)
     deriving (Show, Eq)
     
data Instr a = Assign (Ann Expr a) (Ann Expr a)
             | Sequence (AnnList Instr a)
     deriving (Show, Eq) 
     
data Decl a = Procedure (Ann Name a) (Ann Instr a)
     deriving (Show, Eq)

data Ann elem annot
  = Ann annot (elem annot) 
     deriving (Show, Eq)
     
instance StructuralTraversable elem => StructuralTraversable (Ann elem) where
  structTraverse desc asc f (Ann ann e) = flip Ann <$> (desc *> structTraverse desc asc f e <* asc) <*> f ann
      
newtype AnnList e a = AnnList { fromAnnList :: [Ann e a] }
     deriving (Show, Eq)

instance StructuralTraversable elem => StructuralTraversable (AnnList elem) where
  structTraverse desc asc f (AnnList ls) = AnnList <$> sequenceA (map (structTraverse desc asc f) ls)
     
input = Ann () (Procedure (Ann () (Name "program1")) (Ann () (Sequence (AnnList 
            [ Ann () (Assign (Ann () (Variable (Name "a"))) (Ann () (LitExpr (IntLit 1))))
            , Ann () (Assign (Ann () (Variable (Name "v"))) (Ann () (Plus (Ann () (Variable (Name "b")))
                                       (Ann () (LitExpr (IntLit 2))))))
            ]))))
     
expected = Ann [] (Procedure (Ann [0] (Name "program1")) (Ann [1] (Sequence (AnnList 
            [ Ann [0,1] (Assign (Ann [0,0,1] (Variable (Name "a"))) (Ann [1,0,1] (LitExpr (IntLit 1))))
            , Ann [1,1] (Assign (Ann [0,1,1] (Variable (Name "v"))) (Ann [1,1,1] (Plus (Ann [0,1,1,1] (Variable (Name "b")))
                                       (Ann [1,1,1,1] (LitExpr (IntLit 2))))))
            ]))))
     
deriveStructTrav ''Lit
deriveStructTrav ''Expr
deriveStructTrav ''Instr
deriveStructTrav ''Decl
deriveStructTrav ''Name

main :: IO ()
main = assertEqual "The result of the transformation is not expected" 
                   expected (indexedTraverse (\_ i -> i) input)
     