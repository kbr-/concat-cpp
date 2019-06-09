{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -fplugin=ConCat.Plugin #-}
{-# OPTIONS_GHC -fsimpl-tick-factor=2800 #-}
{-# OPTIONS_GHC -fexpose-all-unfoldings #-}
{-# OPTIONS_GHC -fplugin-opt=ConCat.Plugin:showResiduals #-}

module Main where

import ConCat.AltCat (toCcc)
import ConCat.Rebox ()
import GHC.Generics

import Category

h :: Int -> Int -> Int
h x y = x + y

gun :: Int -> (Int, Int)
gun x = (x, x)

fun :: (Int, Int) -> Int
fun (x, y) = h x y + y

--f :: Int -> Int
--f = fun . gun

f :: Int -> Int -> Int
f x y = x + y

main :: IO ()
main = putStrLn $ runKat 21 (toCcc f)
