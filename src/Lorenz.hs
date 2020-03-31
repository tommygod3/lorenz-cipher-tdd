module Lorenz where

import Data.Char

-- Shift ASCII characters to be Int between 0 and 94
-- All other non alphabetical ASCII characters go to 95 (\DEL)
encodeChar :: Char -> Int
encodeChar x | ord x < 32 = 95
encodeChar x | ord x > 126 = 95
encodeChar x = (ord x) - 32

