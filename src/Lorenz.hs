module Lorenz where

import Data.Char

-- Shift printing ASCII characters (32-126) to be Int between 0 and 94
-- All other non printing ASCII characters go to 95 (\DEL)
encodeChar :: Char -> Int
encodeChar x | ord x < 32 = 95
encodeChar x | ord x > 126 = 95
encodeChar x = (ord x) - 32

decodeChar :: Int -> Char
decodeChar x | x < 0 = chr (127)
decodeChar x | x > 94 = chr (127)
decodeChar x = chr (x + 32)

