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

import Data.Monoid ((<>))
import Data.Foldable (toList)
import Control.Applicative (liftA2)
import Data.List (unfoldr, intercalate)
import Control.Arrow (arr,Kleisli(..))
import qualified Data.Map as M
import Data.Kind (Type)
import Control.Monad.State (State, StateT,execState,runStateT,execStateT,get,gets,put,modify,lift)
import GHC.Types (Constraint)
import ConCat.Category
import ConCat.Misc ((:*),(:+),Unop,Binop,Yes1,typeR,transpose)

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
    = FunComp FunName [Param] [Stmt]

-- Function definition component
funComp :: FunName -> [Param] -> [Stmt] -> Comp
funComp = FunComp

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

    CallFunE :: String -> Expr a -> Expr b

type M = State (Int, [Comp])
newtype Kat a b = Kat { unKat :: Kleisli M (Expr a) (Expr b) }

freshFun :: M String
freshFun = do
    id <- gets fst
    modify (first $ const (id + 1))
    pure $ "f" ++ show id

newComp :: Comp -> M ()
newComp c = modify $ second $ (c :)

pattern K :: (Expr a -> M (Expr b)) -> Kat a b
pattern K f = Kat (Kleisli f)

instance Category Kat where
    id = Kat id
    Kat f . Kat g = Kat (f . g)

instance MonoidalPCat Kat where
    K f *** K g = K $ \eab -> do
        ec <- f $ VarE "x"
        ed <- g $ VarE "y"
        funName <- freshFun
        newComp $ funComp funName [mkParam "xy"]
            [ bindStmt ["x", "y"] eab
            , retStmt $ MakePairE ec ed
            ]
        pure $ CallFunE funName eab

instance ProductCat Kat where
    exl = K $ \eab -> pure $ PairFirstE eab
    exr = K $ \eab -> pure $ PairSecondE eab
    dup = K $ \ea -> do
        let ea' = VarE "x"
        funName <- freshFun
        newComp $ funComp funName [mkParam "x"]
            [ retStmt $ MakePairE ea' ea'
            ]
        pure $ CallFunE funName ea

instance NumCat Kat Int where
    negateC = K $ \ea -> pure $ IntE $ NegE ea
    addC = K $ \eab -> pure $ IntE $ AddE (PairFirstE eab) (PairSecondE eab)
    subC = undefined
    mulC = undefined
    powIC = undefined

printParam :: Param -> String
printParam param = "auto " ++ param

printIntExpr :: IntExpr -> String
printIntExpr (ConstE x) = show x
printIntExpr (AddE e1 e2) = "(" ++ printExpr e1 ++ ") + (" ++ printExpr e2 ++ ")"
printIntExpr (NegE e) = "-(" ++ printExpr e ++ ")"

printExpr :: Expr a -> String
printExpr (IntE ie) = printIntExpr ie
printExpr (VarE v) = v
printExpr (MakePairE ea eb) = "{" ++ printExpr ea ++ ", " ++ printExpr eb ++ "}"
printExpr (PairFirstE e) = printExpr e ++ ".first"
printExpr (PairSecondE e) = printExpr e ++ ".second"
printExpr (CallFunE f e) = f ++ "(" ++ printExpr e ++ ")"

printStmt :: Stmt -> String
printStmt s = "    " ++ printStmt' s ++ ";"

printStmt' :: Stmt -> String
printStmt' (RetStmt ea) = "return " ++ printExpr ea
printStmt' (BindStmt vs ea) = "auto [" ++ intercalate ", " vs ++ "] = " ++ printExpr ea
printStmt' (ExprStmt ea) = printExpr ea

printComp :: Comp -> String
printComp (FunComp name params stmts) =
    "auto " ++ name ++ "(" ++ intercalate ", " (map printParam params) ++ ") {\n" ++
    intercalate "\n" (map printStmt stmts) ++
    "}"

runM :: M a -> String
runM m = intercalate "\n\n" $ map printComp $ snd $ execState m (0, [])

runKat  :: Int -> Kat Int b -> String
runKat x (K f) = runM $ do
    eb <- f (IntE $ ConstE x)
    newComp $ funComp "main" []
        [ exprStmt eb
        ]
