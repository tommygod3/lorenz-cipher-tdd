module Main where

import Data.Char
import Lorenz

main :: IO ()
main = do
    putStr "Message: "
    messageIn <- getLine
    let message = map toUpper messageIn
    putStr "Key: "
    keyIn <- getChar
    let key = toUpper keyIn
    putStrLn("\n")
    print(lorenzCipher message key)
