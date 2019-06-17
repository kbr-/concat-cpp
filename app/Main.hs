{-# LANGUAGE TypeFamilies, TypeOperators, ConstraintKinds #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -fplugin=ConCat.Plugin #-}
{-# OPTIONS_GHC -fsimpl-tick-factor=2800 #-}
{-# OPTIONS_GHC -fexpose-all-unfoldings #-}
{-# OPTIONS_GHC -fplugin-opt=ConCat.Plugin:showResiduals #-}
-- {-# OPTIONS_GHC -fplugin-opt=ConCat.Plugin:trace #-}

module Main where

import ConCat.AltCat (toCcc, inl)
import ConCat.Rebox ()

import Category

h :: Int -> Int -> Int
h x y = x + y

gun :: Int -> (Int, Int)
gun x = (x, x)

fun :: (Int, Int) -> Int
fun x = h (fst x) (snd x) + (snd x)

-- f :: Int -> Int
-- f = fun . gun

-- f :: Int -> (Int, Int) -> Int -> Int
-- f x (y, z) a = x - (y + z) * a

f :: Int -> Int -> Int
f x y = sum [3 + y, y + 1, x + 2]

main :: IO ()
main = putStrLn $ runKat 21 (toCcc f)
