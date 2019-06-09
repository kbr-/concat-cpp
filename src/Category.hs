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
import Control.Arrow (Kleisli(..))
import Data.Kind (Type)
import Control.Monad.State (State, execState,gets,modify)
import ConCat.Category
import ConCat.Misc ((:*))

type VarName = String
type FunName = String

-- Statements
data Stmt :: Type where
    RetStmt :: Expr a -> Stmt
    BindStmt :: [VarName] -> Expr a -> Stmt
    ExprStmt :: Expr a -> Stmt

retStmt :: Expr a -> Stmt
retStmt = RetStmt

bindStmt :: [VarName] -> Expr a -> Stmt
bindStmt = BindStmt

exprStmt :: Expr a -> Stmt
exprStmt = ExprStmt

-- Function parameters
type Param = VarName

mkParam :: VarName -> Param
mkParam = id

-- Components
data Comp
    = FunComp Typ FunName [Param] [Stmt]

data Typ
    = AutoT
    | IntT

-- Function definition component
funComp :: FunName -> [Param] -> [Stmt] -> Comp
funComp = FunComp AutoT

data IntExpr
    = ConstE Int
    | AddE (Expr Int) (Expr Int)
    | NegE (Expr Int)

data Expr :: Type -> Type where
    IntE :: IntExpr -> Expr Int
    VarE :: VarName -> Expr a

    MakePairE :: Expr a -> Expr b -> Expr (a :* b)
    PairFirstE :: Expr (a :* b) -> Expr a
    PairSecondE :: Expr (a :* b) -> Expr b

    LamE :: Param -> [Stmt] -> Expr (a -> b)
    FunE :: FunName -> Expr (a -> b)

    CallFunE :: Expr (a -> b) -> Expr a -> Expr b

type M = State (Int, [Comp])
newtype Kat a b = Kat { unKat :: Kleisli M (Expr a) (Expr b) }

freshFun :: M String
freshFun = do
    x <- gets fst
    modify (first $ const (x + 1))
    pure $ "f" ++ show x

newComp :: Comp -> M ()
newComp c = modify $ second $ (c :)

pattern K :: (Expr a -> M (Expr b)) -> Kat a b
pattern K f = Kat (Kleisli f)

instance Category Kat where
    id = Kat id
    Kat f . Kat g = Kat (f . g)

instance MonoidalPCat Kat where
    Kat (Kleisli f) *** Kat (Kleisli g) = K $ \eab -> do
        ec <- f $ PairFirstE $ VarE "xy"
        ed <- g $ PairSecondE $ VarE "xy"
        funName <- freshFun
        newComp $ funComp funName [mkParam "xy"]
            [ retStmt $ MakePairE ec ed
            ]
        pure $ CallFunE (FunE funName) eab

instance ProductCat Kat where
    exl = K $ \eab -> pure $ PairFirstE eab
    exr = K $ \eab -> pure $ PairSecondE eab
    dup = K $ \ea -> do
        let ea' = VarE "x"
        funName <- freshFun
        newComp $ funComp funName [mkParam "x"]
            [ retStmt $ MakePairE ea' ea'
            ]
        pure $ CallFunE (FunE funName) ea

instance ClosedCat Kat where
    curry (Kat (Kleisli f)) = K $ \ea -> do
        ec <- f $ MakePairE (VarE "x") (VarE "y")
        funName <- freshFun
        newComp $ funComp funName [mkParam "x"]
            [ retStmt $ LamE (mkParam "y")
                [ retStmt ec
                ]
            ]
        pure $ CallFunE (FunE funName) ea
    uncurry (Kat (Kleisli f)) = K $ \eab -> do
        let eab' = VarE "xy"
        efbc <- f $ PairFirstE eab'
        funName <- freshFun
        newComp $ funComp funName [mkParam "xy"]
            [ retStmt $ CallFunE efbc (PairSecondE eab')
            ]
        pure $ CallFunE (FunE funName) eab

instance ConstCat Kat Int where
    const x = K $ const $ pure $ IntE $ ConstE x

instance NumCat Kat Int where
    negateC = K $ \ea -> pure $ IntE $ NegE ea
    addC = K $ \eab -> do
        let eab' = VarE "xy"
        funName <- freshFun
        newComp $ funComp funName [mkParam "xy"]
            [ retStmt $ IntE $ AddE (PairFirstE eab') (PairSecondE eab')
            ]
        pure $ CallFunE (FunE funName) eab
    subC = undefined
    mulC = undefined
    powIC = undefined

indent :: Int -> String -> String
indent level s = concat (replicate level "    ") ++ s

printParam :: Param -> String
printParam param = "auto " ++ param

printIntExpr :: IntExpr -> String
printIntExpr (ConstE x) = show x
printIntExpr (AddE e1 e2) = "(" ++ printExpr 0 e1 ++ ") + (" ++ printExpr 0 e2 ++ ")"
printIntExpr (NegE e) = "-(" ++ printExpr 0 e ++ ")"

printExpr :: Int -> Expr a -> String
printExpr _ (IntE ie) = printIntExpr ie
printExpr _ (VarE v) = v
printExpr l (MakePairE ea eb) = "make_pair(" ++ printExpr l ea ++ ", " ++ printExpr l eb ++ ")"
printExpr l (PairFirstE e) = printExpr l e ++ ".first"
printExpr l (PairSecondE e) = printExpr l e ++ ".second"
printExpr l (LamE p b) = printLam l p b
printExpr _ (FunE s) = s
printExpr l (CallFunE f e) = printExpr l f ++ "(" ++ printExpr l e ++ ")"

printLam :: Int -> Param -> [Stmt] -> String
printLam l p body = "[=] (" ++ printParam p ++ ") {\n" ++ printBody (l + 1) body ++ "\n" ++ indent l "}"

printStmt :: Int -> Stmt -> String
printStmt l s = indent l (printStmt' l s) ++ ";"

printStmt' :: Int -> Stmt -> String
printStmt' l (RetStmt ea) = "return " ++ printExpr l ea
printStmt' l (BindStmt vs ea) = "auto [" ++ intercalate ", " vs ++ "] = " ++ printExpr l ea
printStmt' l (ExprStmt ea) = printExpr l ea

printBody :: Int -> [Stmt] -> String
printBody l = intercalate "\n" . map (printStmt l)

printTyp :: Typ -> String
printTyp AutoT = "auto"
printTyp IntT = "int"

printComp :: Comp -> String
printComp (FunComp typ name params stmts) =
    printTyp typ ++ " " ++ name ++ "(" ++ intercalate ", " (map printParam params) ++ ") {\n" ++
    printBody 1 stmts ++
    "\n}"

runM :: M a -> String
runM m =
    "#include <utility>\n" ++
    "using namespace std;\n\n" ++
    (intercalate "\n\n" $ map printComp $ reverse $ snd $ execState m (0, []))

runKat  :: Int -> Kat Int b -> String
runKat x (Kat (Kleisli f)) = runM $ do
    eb <- f (IntE $ ConstE x)
    newComp $ FunComp IntT "main" []
        [ exprStmt eb
        , retStmt (IntE $ ConstE 0)
        ]
