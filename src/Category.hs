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
import ConCat.Category
import ConCat.Misc ((:*))

type VarId = Int

-- A C++ expression
data Expr :: Type -> Type where
    UnitE :: Expr ()

    IntConstE :: Int -> Expr Int
    NegE :: Expr Int -> Expr Int
    AddE :: Expr Int -> Expr Int -> Expr Int
    SubE :: Expr Int -> Expr Int -> Expr Int
    MulE :: Expr Int -> Expr Int -> Expr Int

    BoolConstE :: Bool -> Expr Bool

    PairE :: Expr a -> Expr b -> Expr (a :* b)
    FirstE :: Expr (a :* b) -> Expr a
    SecondE :: Expr (a :* b) -> Expr b

    LamE :: (Expr a -> Expr b) -> Expr (a -> b)
    CallE :: Expr (a -> b) -> Expr a -> Expr b

    VarE :: Int -> Expr a

newtype Kat a b = Kat { unKat :: Expr a -> Expr b }

instance Category Kat where
 -- id :: a `k` a
 -- (.) :: (b `k` c) -> (a `k` b) -> a `k` c
    id = Kat id
    Kat f . Kat g = Kat (f . g)

instance MonoidalPCat Kat where
 -- (***) :: (a `k` c) -> (b `k` d) -> (a :* b `k` c :* d)
    Kat f *** Kat g = Kat $ \case
        PairE a b -> PairE (f a) (g b)
        e -> PairE (f $ FirstE e) (g $ SecondE e)

instance ProductCat Kat where
 -- exl :: (a :* b) `k` a
 -- exr :: (a :* b) `k` b
 -- dup :: a `k` (a :* a)
    exl = Kat $ \case
        PairE a _ -> a
        e -> FirstE e
    exr = Kat $ \case
        PairE _ b -> b
        e -> SecondE e
    dup = Kat $ \a -> PairE a a

instance ClosedCat Kat where
 -- curry :: (a :* b `k` c) -> a `k` (b :=> c)
 -- uncurry :: (a `k` b :=> c) -> a :* b `k` c
    curry (Kat f) = Kat $ \a -> LamE $ \b -> f $ PairE a b
    uncurry (Kat f) = Kat $ \case
        PairE a b -> case f a of
            LamE g -> g b
            e -> CallE e b
        e -> case f (FirstE e) of
            LamE g -> g (SecondE e)
            e' -> CallE e' (SecondE e)

-- instance TerminalCat Kat where
--     it = K $ const $ pure UnitE

instance ConstCat Kat Int where
 -- const :: Int -> a `k` Int
    const x = Kat $ const (IntConstE x)
instance ConstCat Kat Bool where
    const x = Kat $ const (BoolConstE x)
instance ConstCat Kat () where
    const _ = Kat $ const UnitE

instance NumCat Kat Int where
 -- negateC :: Int `k` Int
 -- addC :: Int :* Int `k` Int
    negateC = Kat NegE
    addC = Kat $ \case
        PairE a b -> AddE a b
        e -> AddE (FirstE e) (SecondE e)
    subC = Kat $ \case
        PairE a b -> SubE a b
        e -> SubE (FirstE e) (SecondE e)
    mulC = Kat $ \case
        PairE a b -> MulE a b
        e -> MulE (FirstE e) (SecondE e)
    powIC = undefined

indent :: Int -> String -> String
indent l s = concat (replicate l "    ") ++ s

printCall :: String -> [String] -> String
printCall f ps = f ++ "(" ++ intercalate ", " ps ++ ")"

data Env = Env
    { lastVar :: VarId
    , level :: Int
    }

printExpr :: Env -> Expr a -> String
printExpr _ UnitE = "Unit{}"

printExpr _ (IntConstE x) = show x
printExpr e (NegE a) = "-(" ++ printExpr e a ++ ")"
printExpr e (AddE a b) = "(" ++ printExpr e a ++ ") + (" ++ printExpr e b ++ ")"
printExpr e (SubE a b) = "(" ++ printExpr e a ++ ") - (" ++ printExpr e b ++ ")"
printExpr e (MulE a b) = "(" ++ printExpr e a ++ ") * (" ++ printExpr e b ++ ")"

printExpr _ (BoolConstE b) = if b then "true" else "false"

printExpr _ (VarE v) = "x" ++ show v

printExpr e (PairE a b) = printCall "std::make_pair" [printExpr e a, printExpr e b]
printExpr e (FirstE x) = printExpr e x ++ ".first"
printExpr e (SecondE x) = printExpr e x ++ ".second"

printExpr e (LamE f) = printLam e f
printExpr e (CallE f a) = printCall (printExpr e f) [printExpr e a]

printCapture :: Env -> String
printCapture e = intercalate ", " $ map (printExpr e . VarE) [0..lastVar e - 1]

printLam :: Env -> (Expr a -> Expr b) -> String
printLam e f =
    "[" ++ printCapture e ++ "] (auto " ++ printExpr e (VarE $ lastVar e) ++ ") {\n" ++
    indent (level e') "return " ++ printExpr e' (f $ VarE $ lastVar e) ++ ";\n" ++
    indent (level e) "}"
  where
    e' = e { lastVar = lastVar e + 1, level = level e + 1 }

printConst :: Kat () a -> String
printConst (Kat f) = printExpr (Env 0 1) $ f UnitE

runKat :: ConstCat Kat a => a -> Kat a b -> String
runKat x (Kat f) =
    "#include <utility>\n" ++
    "using namespace std;\n\n" ++
    "auto f(auto x0) {\n" ++
    indent 1 ("return " ++ printExpr (Env 1 1) (f $ VarE 0)) ++ ";\n" ++
    "}\n\n" ++
    "int main() {\n" ++
    indent 1 (printCall "f" [printConst $ const x]) ++ ";\n" ++
    "}"
