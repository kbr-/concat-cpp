{-# LANGUAGE TypeFamilies, TypeOperators, ConstraintKinds #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -fplugin=ConCat.Plugin #-}
{-# OPTIONS_GHC -fsimpl-tick-factor=2800 #-}
{-# OPTIONS_GHC -fexpose-all-unfoldings #-}
{-# OPTIONS_GHC -fplugin-opt=ConCat.Plugin:showResiduals #-}
{-# OPTIONS_GHC -fplugin-opt=ConCat.Plugin:trace #-}

module Main where

import ConCat.Misc
import ConCat.AltCat (toCcc, inl)
import ConCat.Rebox ()
import ConCat.Rep
import GHC.Generics (U1(..),Par1(..),(:*:)(..),(:.:)(..))

import Category

h :: Int -> Int -> Int
h x y = x + y

gun :: Int -> (Int, Int)
gun x = (x, x)

fun :: (Int, Int) -> Int
fun x = h (fst x) (snd x) + (snd x)

-- f :: Int -> Int
-- f = fun . gun

-- f :: Int -> Int -> Int
-- f x y = x + y

-- f :: Int -> Either Int Int
-- f x = inl x

-- g :: [Int] -> Int
-- g [] = 0
-- g (c:cs) = c + g cs
-- {-# NOINLINE g #-}

-- f :: Int -> Int
-- f x = g [1,2,3]
-- {-# NOINLINE f #-}

-- f :: Int -> List
-- f x = Nil

f :: Int -> Either Int Bool
f x = inl x

-- f :: Int -> E
-- f x = L

-- f :: Int -> X
-- f x = X

-- f :: Int -> (Int, Int, Int)
-- f x = (x, x, x)

--f :: Int -> List
--f x | x > 0 = Cons x (f (x - 1))
--f _ = Nil

main :: IO ()
main = putStrLn $ runKat 21 (toCcc f)
