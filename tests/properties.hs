{-# LANGUAGE TemplateHaskell #-}
module Main where

import Test.Framework.TH

main :: IO ()
main = $defaultMainGenerator