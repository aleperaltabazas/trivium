module Main where

import System.Environment
import Data.Int
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Data.Int
import Numeric (showHex)

import Common
import Trivium

epoch :: IO NominalDiffTime
epoch = getPOSIXTime 

main :: IO ()
main = do
    let key = "80000000000000000000"
    let iv = "00000000000000000000"
    putStrLn "Reading input..."
    let plainText = parseToWord8 "0ffffffffff0ffff0ffff00ffffff0ff0ffffff000ffffff00ff0ffff0ff0ff000000ffffff0ffffffffffffffffffffffffff00ff00ff0ffffff00ff0000ff0ffff0ffffffffff000ffffff0ffff0ff0ff0ffff00000ff00ff0ffff0ff0ff0ff0ff0ff0000ff000ffffffff0ffffff000ff00ff000000ff00ff00ffff0ffffff00ffff0ff0ffff00ffff000ff0ffffffffff0ffffffffffff000ffff0000ff0ff0ffff00ff000ffffff00ff0ffff0ffff00ffff0ff0ff000ff00ff000ff0000ffffffffffffff0ffffffff0ffffff000ffff0ff0ff000ff0ff0ff000000ff0ff0ffff0000ff0ff0000ff00ffffffffff0ffffffffff00000ffffff00ffffff00ffffffff0ffffff0000ff00ffffffffff0ff0ffffff00ff00ff0ffffff0ffffffffffffffffffffff000ffff00ffff000ff000000000000ffffffffffffffffffff0ffff0ff0ff00000ff0ff00000ff0ff0ffff000ffffff0ff000ff0000ffff0ffff0ff000000ffffff000ffff0000ff0ffff0ff0000ff000000ffff0ffff0"
    putStrLn "Encrypting..."
    start <- epoch
    print start
    let cipherText = encrypt plainText (concatMap toBits . readHexString $ key) (concatMap toBits . readHexString $ iv)
    end <- epoch
    print end
    putStrLn ("Encryption done in " ++ (show (end - start)) ++ " millis")
    print . fromHex $ cipherText