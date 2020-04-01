module Lorenz where

import Data.Char
import Data.Bits

-- Shift printing ASCII characters (32-95) to be Int between 0 and 94
-- All other non printing ASCII characters go to 63 (_)
encodeChar :: Char -> Int
encodeChar x | ord x < 32 = 63
encodeChar x | ord x > 95 = 63
encodeChar x = (ord x) - 32

decodeChar :: Int -> Char
decodeChar x | x < 0 = chr (95)
decodeChar x | x > 94 = chr (95)
decodeChar x = chr (x + 32)

vernamCipher :: Char -> Char -> Char
vernamCipher input key = decodeChar (xor (encodeChar input) (encodeChar key))

