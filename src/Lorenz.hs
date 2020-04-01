module Lorenz where

import Data.Char
import Data.Bits
import System.Random

-- Shift range of ASCII characters (32-95) to be Int between 0 and 94
-- All other ASCII characters go to 63 (_)
-- This means that all characters not between 32 and 95 will be deciphered as _
encodeChar :: Char -> Int
encodeChar x | ord x < 32 = 63
encodeChar x | ord x > 95 = 63
encodeChar x = (ord x) - 32

decodeChar :: Int -> Char
decodeChar x | x < 0 = chr (63)
decodeChar x | x > 63 = chr (63)
decodeChar x = chr (x + 32)

vernamCipher :: Char -> Char -> Char
vernamCipher input key = decodeChar (xor (encodeChar input) (encodeChar key))

seededRandomChar :: Char -> (Char, StdGen)
seededRandomChar seed = randomR ('\32', '\95') (mkStdGen (ord seed)) :: (Char, StdGen)

randomChar :: (Char, StdGen) -> (Char, StdGen)
randomChar (previous, seed) = randomR ('\32', '\95') seed :: (Char, StdGen)
