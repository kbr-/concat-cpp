{-# LANGUAGE TypeFamilies, TypeOperators, ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts, MultiParamTypeClasses #-}
{-# LANGUAGE ViewPatterns, TupleSections #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, StandaloneDeriving #-}
{-# LANGUAGE ExistentialQuantification, TypeSynonymInstances, GADTs #-}
{-# LANGUAGE Rank2Types, ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE DeriveFunctor, DeriveDataTypeable #-}
{-# LANGUAGE TypeApplications, AllowAmbiguousTypes #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE NoStarIsType #-}

{-# OPTIONS_GHC -Wall #-}

module Category where

import Prelude hiding (id,(.),const)

import Data.List (intercalate)
import Data.Kind (Type)
import Data.Constraint (Dict(..),(:-)(..))
import Control.Arrow (Kleisli(..))
import Control.Monad.Identity
import ConCat.Category
import ConCat.Misc ((:*),(:+))

class IsTyp a where
    printTyp :: String

instance IsTyp Int where
    printTyp = "int"

instance IsTyp Bool where
    printTyp = "bool"

instance IsTyp () where
    printTyp = "Unit"

instance (IsTyp a, IsTyp b) => IsTyp (a :* b) where
    printTyp = "std::pair<" ++ printTyp @a ++ ", " ++ printTyp @b ++ ">"

instance (IsTyp a, IsTyp b) => IsTyp (a :+ b) where
    printTyp = "Sum<" ++ printTyp @a ++ ", " ++ printTyp @b ++ ">"

instance (IsTyp a, IsTyp b) => IsTyp (a -> b) where
    -- can we do better?
    printTyp = "std::function<" ++ printTyp @b ++ "(" ++ printTyp @a ++ ")>"

type VarName = String
type FunName = String

-- Statements
data Stmt :: Type where
    RetStmt :: Expr a -> Stmt

-- Function parameters
type Param = VarName

mkParam :: VarName -> Param
mkParam = id

-- A C++ expression
data Expr :: Type -> Type where
    UnitE :: Expr ()

    IntConstE :: Int -> Expr Int
    NegE :: Expr Int -> Expr Int
    AddE :: Expr (Int :* Int) -> Expr Int

    BoolConstE :: Bool -> Expr Bool

    VarE :: VarName -> Expr a

    CombineE :: Expr (a -> c) -> Expr (b -> d) -> Expr (a :* b) -> Expr (c :* d)

    PairFirstE :: Expr (a :* b) -> Expr a
    PairSecondE :: Expr (a :* b) -> Expr b
    DupE :: Expr a -> Expr (a :* a)

    CurryE :: Expr (a :* b -> c) -> Expr a -> Expr (b -> c)
    UncurryE :: Expr (a -> b -> c) -> Expr (a :* b) -> Expr c

    SplitE :: Expr (c -> a) -> Expr (d -> b) -> Expr (c :+ d) -> Expr (a :+ b)

    InlE :: IsTyp b => Expr a -> Expr (a :+ b)
    InrE :: IsTyp a => Expr b -> Expr (a :+ b)
    JamE :: Expr (a :+ a) -> Expr a

    LamE :: Param -> [Stmt] -> Expr (a -> b)

type M = Identity -- for now...
newtype Kat a b = Kat { unKat :: Kleisli M (Expr a) (Expr b) }

pattern K :: (Expr a -> M (Expr b)) -> Kat a b
pattern K f = Kat (Kleisli f)

toLamE :: Kat a b -> M (Expr (a -> b))
toLamE (Kat (Kleisli f)) = f (VarE "x") >>= \eb -> pure $ LamE (mkParam "x") [RetStmt eb]

instance Category Kat where
    type Ok Kat = IsTyp
    id = Kat id
    Kat f . Kat g = Kat (f . g)

instance OpCon (:*) (Sat IsTyp) where inOp = Entail (Sub Dict)
instance OpCon (:+) (Sat IsTyp) where inOp = Entail (Sub Dict)
instance OpCon (->) (Sat IsTyp) where inOp = Entail (Sub Dict)

instance MonoidalPCat Kat where
    f *** g = K $ \eab -> CombineE <$> toLamE f <*> toLamE g <*> pure eab

instance ProductCat Kat where
    exl = K $ pure . PairFirstE
    exr = K $ pure . PairSecondE
    dup = K $ pure . DupE

instance ClosedCat Kat where
    curry f = K $ \ea -> CurryE <$> toLamE f <*> pure ea
    uncurry f = K $ \eab -> UncurryE <$> toLamE f <*> pure eab

instance TerminalCat Kat where
    it = K $ const $ pure UnitE

instance MonoidalSCat Kat where
    f +++ g = K $ \eab -> SplitE <$> toLamE f <*> toLamE g <*> pure eab

instance CoproductCat Kat where
    inl = K $ pure . InlE
    inr = K $ pure . InrE
    jam = K $ pure . JamE

instance ConstCat Kat Int where
    const x = K $ const $ pure $ IntConstE x
instance ConstCat Kat Bool where
    const x = K $ const $ pure $ BoolConstE x
instance ConstCat Kat () where
    const _ = K $ const $ pure UnitE

instance NumCat Kat Int where
    negateC = K $ pure . NegE
    addC = K $ pure . AddE
    subC = undefined
    mulC = undefined
    powIC = undefined

indent :: Int -> String -> String
indent level s = concat (replicate level "    ") ++ s

printParam :: Param -> String
printParam param = "auto " ++ param

printCall :: String -> [String] -> String
printCall f ps = f ++ "(" ++ intercalate ", " ps ++ ")"

printExpr :: Int -> Expr a -> String
printExpr _ UnitE = "Unit{}"

printExpr _ (IntConstE x) = show x
printExpr l (NegE e) = "-(" ++ printExpr l e ++ ")"
printExpr l (AddE e) = printCall "add" [printExpr l e]

printExpr _ (BoolConstE b) = if b then "true" else "false"

printExpr _ (VarE v) = v

printExpr l (CombineE f g ab) = printCall "combine" [printExpr l f, printExpr l g, printExpr l ab]

printExpr l (PairFirstE e) = printExpr l e ++ ".first"
printExpr l (PairSecondE e) = printExpr l e ++ ".second"
printExpr l (DupE e) = printCall "dupl" [printExpr l e]

printExpr l (CurryE f a) = printCall "curry" [printExpr l f, printExpr l a]
printExpr l (UncurryE f ab) = printCall "uncurry" [printExpr l f, printExpr l ab]

printExpr l (SplitE f g cd) = printCall "split" [printExpr l f, printExpr l g, printExpr l cd]

-- C++ needs to be given the type of the other element of the pair.
printExpr l e@(InlE _) = printInl l e
printExpr l e@(InrE _) = printInr l e
printExpr l (JamE e) = printCall "jam" [printExpr l e]

printExpr l (LamE p b) = printLam l p b

printInl :: forall a b. IsTyp b => Int -> Expr (a :+ b) -> String
printInl l (InlE e) = printCall ("inl<" ++ printTyp @b ++ ">") [printExpr l e]
printInl _ _ = error "printInl"

printInr :: forall a b. IsTyp a => Int -> Expr (a :+ b) -> String
printInr l (InlE e) = printCall ("inr<" ++ printTyp @a ++ ">") [printExpr l e]
printInr _ _ = error "printInr"

printLam :: Int -> Param -> [Stmt] -> String
printLam l p body =
    "[=] (" ++ printParam p ++ ") {\n" ++
    printBody (l + 1) body ++ "\n" ++
    indent l "}"

printStmt :: Int -> Stmt -> String
printStmt l s = indent l (printStmt' l s) ++ ";"

printStmt' :: Int -> Stmt -> String
printStmt' l (RetStmt ea) = "return " ++ printExpr l ea

printBody :: Int -> [Stmt] -> String
printBody l = intercalate "\n" . map (printStmt l)

runM :: Kat () a -> String
runM (Kat (Kleisli f)) = printExpr 1 . runIdentity $ f UnitE

runKatExpr :: (IsTyp a, IsTyp b, ConstCat Kat a) => a -> Kat a b -> String
runKatExpr x f = runM $ f . const x

runKat :: (IsTyp a, IsTyp b, ConstCat Kat a) => a -> Kat a b -> String
runKat x f =
    "#include \"lib.h\"\n" ++
    "using namespace std;\n\n" ++
    "int main() {\n" ++
    "    " ++ runKatExpr x f ++ ";\n" ++
    "}"
