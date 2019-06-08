{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -fplugin=ConCat.Plugin #-}
{-# OPTIONS_GHC -fsimpl-tick-factor=2800 #-}
{-# OPTIONS_GHC -fexpose-all-unfoldings #-}

module Main where

import ConCat.AltCat (toCcc)
import ConCat.Rebox ()
import Prelude hiding ((.), id, curry, uncurry)

import Category

fun :: Num a => a -> a
fun x = x + x

main :: IO ()
main = putStrLn $ runKat 21 (toCcc (fun @Int))
