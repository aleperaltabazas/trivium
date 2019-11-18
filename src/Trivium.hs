module Trivium where

import System.Environment (getProgName, getArgs)
import System.Directory (doesFileExist)
import qualified Data.ByteString as B
import Data.Maybe (fromJust)
import Data.List.Split (chunksOf)
import Data.Char (toUpper)
import Numeric (readHex)
import Data.Word
import Data.Bits (xor, (.&.), Bits)
import Common

type Key = [Bit]
type IV = [Bit]
type State = [Bit]

(!) :: Bits a => a -> a -> a
(!) = xor

a & b = a .&. b

setUp :: Key -> IV -> State
setUp key iv = (setUpKey key) ++ (setUpIV iv) ++ finalPadding

setUpKey :: Key -> State
setUpKey key = key ++ (replicate 13 False)

setUpIV :: IV -> State
setUpIV iv = iv ++ (replicate 4 False)

finalPadding :: State
finalPadding = (replicate 108 False) ++ (replicate 3 True)

rotationRound :: State -> State
rotationRound state =
    let t1 = (state !! 65)  ! ((state !! 91)  & (state !! 91))  ! (state !! 92)  ! (state !! 170)
        t2 = (state !! 161) ! ((state !! 174) & (state !! 175)) ! (state !! 176) ! (state !! 263)
        t3 = (state !! 242) ! ((state !! 285) & (state !! 286)) ! (state !! 287) ! (state !! 68)

        (xs, ys, zs) = splitAt2 93 177 state
    in
        t3:(init xs) ++ t1:(init ys) ++ t2:(init zs)

initialize :: Key -> IV -> State
initialize key iv = foldl (\state _ -> rotationRound state) (setUp key iv) [1..(4 * 288)]

keyStreamGenerationRound :: State -> (Bit, State)
keyStreamGenerationRound state =
    let t1 = (state !! 65) ! (state !! 92)
        t2 = (state !! 161) ! (state !! 176)
        t3 = (state !! 242) ! (state !! 287)

        z = t1 ! t2 ! t3

        t1' = t1 ! ((state !! 90) & (state !! 91)) ! (state !! 170)
        t2' = t2 ! ((state !! 174) & (state !! 175)) ! (state !! 263)
        t3' = t3 ! ((state !! 285) & (state !! 286)) ! (state !! 68)

        (xs, ys, zs) = splitAt2 93 177 state
    in
        (z, t3':(init xs) ++ t1':(init ys) ++ t2':(init zs))

keyStream :: Int -> State -> ([Bit], State)
keyStream size state = 
    foldl (\(accumulatedBits, currentState) _ -> mapFst (\x -> accumulatedBits ++ [x]) . keyStreamGenerationRound $ state) 
    ([], state) (replicate size 0)

nextByte :: State -> (Word8, State)
nextByte state = mapFst bitsToBytes . keyStream 8 $ state

cipherRound :: Word8 -> State -> (Word8, State)
cipherRound plainByte state = mapFst (! plainByte) . nextByte $ state

cipherIteration :: ([Word8], State) -> Word8 -> ([Word8], State)
cipherIteration (cipherText, state) plainByte = mapFst (\x -> cipherText ++ [x]) . cipherRound plainByte $ state

cipher :: State -> [Word8] -> ([Word8], State)
cipher state plainText = foldl cipherIteration ([], state) plainText

encrypt :: [Word8] -> Key -> IV -> [Word8]
encrypt plainText key iv =
    let state = initialize key iv
    in
        fst . cipher state $ plainText