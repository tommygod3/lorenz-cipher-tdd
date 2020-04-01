{-# LANGUAGE ScopedTypeVariables, TemplateHaskell #-}
module Main where

-- imports

import Test.QuickCheck

import Data.Char
import Lorenz

-- tests

-- encodeChar
prop_encodeCharInRange :: Char -> Bool
prop_encodeCharInRange x = 
    encodeChar x <= 95

prop_encodeCharPositive :: Char -> Bool
prop_encodeCharPositive x = 
    0 <= encodeChar x

prop_encodeChar :: Char -> Bool
prop_encodeChar x = 
    encodeChar x == 
        if (32 <= ord x) && (ord x <= 126)
            then (ord x) - 32
        else 95

-- decodeChar
prop_decodeCharInRange :: Int -> Bool
prop_decodeCharInRange x = 
    ord (decodeChar x) <= 127

prop_decodeCharPositive :: Int -> Bool
prop_decodeCharPositive x = 
    0 <= ord (decodeChar x)

prop_decodeCharIsAscii :: Int -> Bool
prop_decodeCharIsAscii x = 
    isAscii (decodeChar x)

prop_decodeChar :: Int -> Bool
prop_decodeChar x = 
    decodeChar x == 
        if (0 <= x) && (x <= 94)
            then chr (x + 32)
        else chr 127

-- vernam cipher
prop_vernamCipherIsAscii :: Char -> Char -> Bool
prop_vernamCipherIsAscii input key =
    isAscii (vernamCipher input key)

prop_vernamCipherInputChanges :: Char -> Char -> Bool
prop_vernamCipherInputChanges input key =
    vernamCipher input key /= vernamCipher (decodeChar (rem ((encodeChar input) + 1) 95)) key

-- main
return []
main = $quickCheckAll
