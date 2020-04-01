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

data SeedPair = SeedPair (Char, StdGen) deriving (Show, Read)

getChr :: SeedPair -> Char
getChr (SeedPair pair) = fst pair

getSeed :: SeedPair -> StdGen
getSeed (SeedPair pair) = snd pair

seededRandomChar :: Char -> SeedPair
seededRandomChar seed =
    SeedPair (randomR ('\32', '\95') (mkStdGen (ord seed)) :: (Char, StdGen))

randomChar :: SeedPair -> SeedPair
randomChar seedPair = 
    SeedPair (randomR ('\32', '\95') (getSeed seedPair) :: (Char, StdGen))

lorenzCipher :: String -> Char -> String
lorenzCipher input key = fst $ foldr (\x (decoded, seedPair) -> 
                ((vernamCipher x (getChr seedPair) : decoded), (randomChar seedPair)))
                ([], (seededRandomChar key)) input
