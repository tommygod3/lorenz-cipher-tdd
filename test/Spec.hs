{-# LANGUAGE ScopedTypeVariables, TemplateHaskell #-}
module Main where

-- imports

import Test.QuickCheck

import Lorenz

-- tests


-- main
return []
main = $quickCheckAll
