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

    LamE :: Param -> [Stmt] -> Expr (a -> b)

newtype Kat a b = Kat { unKat :: Expr a -> Expr b }

toLamE :: Kat a b -> Expr (a -> b)
toLamE (Kat f) = LamE (mkParam "x") [RetStmt $ f (VarE "x")]

instance Category Kat where
 -- id :: a `k` a
 -- (.) :: (b `k` c) -> (a `k` b) -> a `k` c
    id = Kat id
    Kat f . Kat g = Kat (f . g)

instance MonoidalPCat Kat where
 -- (***) :: (a `k` c) -> (b `k` d) -> (a :* b `k` c :* d)
    f *** g = Kat $ CombineE (toLamE f) (toLamE g)

instance ProductCat Kat where
 -- exl :: (a :* b) `k` a
 -- exr :: (a :* b) `k` b
 -- dup :: a `k` (a :* a)
    exl = Kat PairFirstE
    exr = Kat PairSecondE
    dup = Kat DupE

instance ClosedCat Kat where
 -- curry :: (a :* b `k` c) -> a `k` (b :=> c)
 -- uncurry :: (a `k` b :=> c) -> a :* b `k` c
    curry f = Kat $ CurryE (toLamE f)
    uncurry f = Kat $ UncurryE (toLamE f)

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
    addC = Kat AddE
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

printExpr l (LamE p b) = printLam l p b

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
runM (Kat f) = printExpr 1 $ f UnitE

runKatExpr :: ConstCat Kat a => a -> Kat a b -> String
runKatExpr x f = runM $ f . const x

runKat :: ConstCat Kat a => a -> Kat a b -> String
runKat x f =
    "#include \"lib.h\"\n" ++
    "using namespace std;\n\n" ++
    "int main() {\n" ++
    "    " ++ runKatExpr x f ++ ";\n" ++
    "}"
