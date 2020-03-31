{-# LANGUAGE ScopedTypeVariables, TemplateHaskell #-}
module Main where

-- imports

import Test.QuickCheck

-- tests

prop_reverseLength :: [Int] -> Bool
prop_reverseLength xs = 
    length xs == length (reverse xs)

-- main
return []
main = $quickCheckAll
