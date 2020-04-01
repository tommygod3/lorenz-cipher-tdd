{-# LANGUAGE TemplateHaskell #-}
module Main where

-- imports

import Test.QuickCheck

import Data.Char
import Lorenz

-- tests

-- encodeChar
prop_encodeCharInRange :: Char -> Bool
prop_encodeCharInRange x = 
    encodeChar x < 64

prop_encodeCharPositive :: Char -> Bool
prop_encodeCharPositive x = 
    0 <= encodeChar x

prop_encodeChar :: Char -> Bool
prop_encodeChar x = 
    encodeChar x == 
        if (32 <= ord x) && (ord x <= 95)
            then (ord x) - 32
        else 63

-- decodeChar
prop_decodeCharInRange :: Int -> Bool
prop_decodeCharInRange x = 
    ord (decodeChar x) <= 95

prop_decodeCharPositive :: Int -> Bool
prop_decodeCharPositive x = 
    0 <= ord (decodeChar x)

prop_decodeCharIsAscii :: Int -> Bool
prop_decodeCharIsAscii x = 
    isAscii (decodeChar x)

prop_decodeChar :: Int -> Bool
prop_decodeChar x = 
    decodeChar x == 
        if (0 <= x) && (x < 64)
            then chr (x + 32)
        else chr 63

-- vernamCipher
prop_vernamCipherIsAscii :: Char -> Char -> Bool
prop_vernamCipherIsAscii input key =
    isAscii (vernamCipher input key)

prop_vernamCipherInputChanges :: Char -> Char -> Bool
prop_vernamCipherInputChanges input key =
    if ((32 <= ord input) && (ord input <= 95)) && ((32 <= ord key) && (ord key <= 95))
        then vernamCipher input key /= vernamCipher (decodeChar (rem ((encodeChar input) + 1) 63)) key
    else if ((32 <= ord input) && (ord input <= 95)) || ((32 <= ord key) && (ord key <= 95))
        then (32 <= ord (vernamCipher input key)) && (ord (vernamCipher input key) <= 95)
    else vernamCipher input key == ' '

prop_vernamCipherSymmetry :: Char -> Char -> Bool
prop_vernamCipherSymmetry input key =
    vernamCipher input key == vernamCipher key input

prop_vernamCipherReciprocity :: Char -> Char -> Bool
prop_vernamCipherReciprocity input key =
    if ((32 <= ord input) && (ord input <= 95)) && ((32 <= ord key) && (ord key <= 95))
        then
            (vernamCipher (vernamCipher input key) key == input)
            && (vernamCipher (vernamCipher input key) input == key)
    else if ((32 <= ord input) && (ord input <= 95)) && not ((32 <= ord key) && (ord key <= 95))
        then
            (vernamCipher (vernamCipher input key) key == input)
            && (vernamCipher (vernamCipher input key) input == '_')
    else if not ((32 <= ord input) && (ord input <= 95)) && ((32 <= ord key) && (ord key <= 95))
        then
            (vernamCipher (vernamCipher input key) key == '_')
            && (vernamCipher (vernamCipher input key) input == key)
    else
        (vernamCipher (vernamCipher input key) key == '_')
        && (vernamCipher (vernamCipher input key) input == '_')

-- seededRandomChar
prop_seededRandomCharIsAscii :: Char -> Bool
prop_seededRandomCharIsAscii seed = 
    isAscii (fst (seededRandomChar seed))

-- main
return []
main = $quickCheckAll
