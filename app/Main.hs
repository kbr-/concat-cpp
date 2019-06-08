{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -fplugin=ConCat.Plugin #-}
{-# OPTIONS_GHC -fsimpl-tick-factor=2800 #-}
{-# OPTIONS_GHC -fexpose-all-unfoldings #-}

module Main where

import ConCat.AltCat (toCcc)
import Prelude hiding ((.), id, curry, uncurry)

import Category

fun :: a -> a
fun x = x

main :: IO ()
main = print $ runKat 42 (toCcc (fun @Int))
