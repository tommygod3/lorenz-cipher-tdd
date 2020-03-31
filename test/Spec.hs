{-# LANGUAGE ScopedTypeVariables, TemplateHaskell #-}
module Main where

-- imports

import Test.QuickCheck

import Data.Char
import Lorenz

-- tests

prop_encodeCharInRange :: Char -> Bool
prop_encodeCharInRange x = 
    encodeChar x <= 95

prop_encodeCharPositive :: Char -> Bool
prop_encodeCharPositive x = 
    0 <= encodeChar x

prop_encodeChar :: Char -> Bool
prop_encodeChar x = 
    chr (encodeChar x) == 
        if (32 <= ord x) && (ord x <= 126)
            then chr ((ord x) - 32) 
        else chr 127

-- main
return []
main = $quickCheckAll
