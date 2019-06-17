{-# LANGUAGE TypeFamilies, TypeOperators #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts, MultiParamTypeClasses #-}
{-# LANGUAGE ViewPatterns, TupleSections #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
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
    IntConstE :: Int -> Expr Int
    NegE :: Expr Int -> Expr Int
    AddE :: Expr Int -> Expr Int -> Expr Int
    SubE :: Expr Int -> Expr Int -> Expr Int
    MulE :: Expr Int -> Expr Int -> Expr Int

    BoolConstE :: Bool -> Expr Bool
    NotE :: Expr Bool -> Expr Bool
    AndE :: Expr Bool -> Expr Bool -> Expr Bool
    OrE :: Expr Bool -> Expr Bool -> Expr Bool
    XorE :: Expr Bool -> Expr Bool -> Expr Bool

    IfE :: Expr Bool -> Expr a -> Expr a -> Expr a

    EqE :: Expr a -> Expr a -> Expr Bool

    LtE :: Expr Int -> Expr Int -> Expr Bool
    GtE :: Expr Int -> Expr Int -> Expr Bool
    LeE :: Expr Int -> Expr Int -> Expr Bool
    GeE :: Expr Int -> Expr Int -> Expr Bool

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

mapPair :: (Expr a -> Expr b -> Expr c) -> Expr (a :* b) -> Expr c
mapPair f (PairE a b) = f a b
mapPair f e = f (FirstE e) (SecondE e)

liftE2 :: (Expr a -> Expr b -> Expr c) -> Kat (a :* b) c
liftE2 = Kat . mapPair

instance BoolCat Kat where
 -- notC :: Bool `k` Bool
 -- andC :: Bool :* Bool `k` Bool
    notC = Kat NotE
    andC = liftE2 AndE
    orC  = liftE2 OrE
    xorC = liftE2 XorE

instance NumCat Kat Int where
 -- negateC :: Int `k` Int
 -- addC :: Int :* Int `k` Int
    negateC = Kat NegE
    addC = liftE2 AddE
    subC = liftE2 SubE
    mulC = liftE2 MulE
    powIC = undefined

instance IfCat Kat a where
 -- ifC :: Bool :* (a :* a) `k` a
    ifC = liftE2 $ mapPair . IfE

instance EqCat Kat a where
 -- equal :: a :* a `k` Bool
    equal = liftE2 EqE

instance OrdCat Kat Int where
 -- lessThan :: Int :* Int `k` Bool
    lessThan = liftE2 LtE
    greaterThan = liftE2 GtE
    lessThanOrEqual = liftE2 LeE
    greaterThanOrEqual = liftE2 GeE

indent :: Int -> String -> String
indent l s = concat (replicate l "    ") ++ s

printCall :: String -> [String] -> String
printCall f ps = f ++ "(" ++ intercalate ", " ps ++ ")"

data Env = Env
    { lastVar :: VarId
    , level :: Int
    }

printBinOp :: Env -> Expr a -> Expr b -> String -> String
printBinOp e a b o = "(" ++ printExpr e a ++ " " ++ o ++ " " ++ printExpr e b ++ ")"

printUnOp :: Env -> Expr a -> String -> String
printUnOp e a o = "(" ++ o ++ printExpr e a ++ ")"

printExpr :: Env -> Expr a -> String
printExpr _ (IntConstE x) = show x
printExpr e (NegE a) = printUnOp e a "-"
printExpr e (AddE a b) = printBinOp e a b "+"
printExpr e (SubE a b) = printBinOp e a b "-"
printExpr e (MulE a b) = printBinOp e a b "*"

printExpr _ (BoolConstE b) = if b then "true" else "false"
printExpr e (NotE a) = printUnOp e a "!"
printExpr e (AndE a b) = printBinOp e a b "&&"
printExpr e (OrE a b) = printBinOp e a b "||"
printExpr e (XorE a b) = printBinOp e a b "^"

printExpr e (IfE b x y) = "(" ++ printExpr e b ++ " ? " ++ printExpr e x ++ " : " ++ printExpr e y ++ ")"

printExpr e (EqE x y) = printBinOp e x y "=="

printExpr e (LtE a b) = printBinOp e a b "<"
printExpr e (GtE a b) = printBinOp e a b ">"
printExpr e (LeE a b) = printBinOp e a b "<="
printExpr e (GeE a b) = printBinOp e a b ">="

printExpr e (PairE a b) = printCall "std::make_pair" [printExpr e a, printExpr e b]
printExpr e (FirstE x) = printExpr e x ++ ".first"
printExpr e (SecondE x) = printExpr e x ++ ".second"

printExpr e (LamE f) = printLam e f
printExpr e (CallE f a) = printCall (printExpr e f) [printExpr e a]

printExpr _ (VarE v) = "x" ++ show v

printCapture :: Env -> String
printCapture e = intercalate ", " $ map (printExpr e . VarE) [0..lastVar e - 1]

printLam :: Env -> (Expr a -> Expr b) -> String
printLam e f =
    "[" ++ printCapture e ++ "] (auto " ++ printExpr e (VarE $ lastVar e) ++ ") {\n" ++
    indent (level e') "return " ++ printExpr e' (f $ VarE $ lastVar e) ++ ";\n" ++
    indent (level e) "}"
  where
    e' = e { lastVar = lastVar e + 1, level = level e + 1 }

printConst :: ConstCat Kat a => a -> String
printConst x = case const x of Kat f -> printExpr (Env 0 1) (f $ IntConstE 0)

runKat :: ConstCat Kat a => a -> Kat a b -> String
runKat x (Kat f) =
    "#include <utility>\n" ++
    "using namespace std;\n\n" ++
    "auto f(auto x0) {\n" ++
    indent 1 ("return " ++ printExpr (Env 1 1) (f $ VarE 0)) ++ ";\n" ++
    "}\n\n" ++
    "int main() {\n" ++
    indent 1 (printCall "f" [printConst x]) ++ ";\n" ++
    "}"
