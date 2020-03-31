{-# LANGUAGE ScopedTypeVariables, TemplateHaskell #-}
module Main where

-- imports

import Test.QuickCheck

import Lorenz

-- tests

prop_encodeCharInRange :: Char -> Bool
prop_encodeCharInRange x = 
    0 <= encodeChar x <= 94

prop_encodeChar :: Char -> Bool
prop_encodeChar x = 
    chr (encodeChar x) == chr ((ord x) - 32)

-- main
return []
main = $quickCheckAll
