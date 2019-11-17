module Encryption where

import System.Environment (getProgName, getArgs)
import System.Directory (doesFileExist)
import qualified Data.ByteString as B
import Data.Maybe (fromJust)
import Data.List.Split (chunksOf)
import Data.Char (toUpper)
import Numeric (readHex)
import Data.Word
import Data.Bits
import Common

type Key = [Bit]
type IV = [Bit]
type State = [Bit]

encrypt :: String -> String -> Key -> IO [Word8]
encrypt input output key = do
    let plainText = B.unpack <$> B.readFile input
    return []

fillInternalState :: Key -> IV -> State 
fillInternalState key initializationVector = padOnes (288 - 285) 
    . padZeroes (285 - 177) 
    . padZeroes (177 - 93 - 80) 
    . fillInitializationVector initializationVector 
    . padZeroes (93 - 80) 
    . fillKey key 
    $ []

fillKey :: Key -> State -> State
fillKey key state = state ++ (take 80 key)

fillInitializationVector :: IV -> State -> State
fillInitializationVector vector state = state ++ take 80 vector

padZeroes :: Int -> State -> State
padZeroes = padWith False

padOnes :: Int -> State -> State
padOnes = padWith True

padWith :: Bit -> Int -> State -> State
padWith x amount state = state ++ (replicate amount x)

shiftAndReplace :: Bit -> Int -> Int -> State -> State
shiftAndReplace replacement from to state = 
    let (beginning, tail) = splitAt from state
        (center, fin) = splitAt (to - from) tail
    in
        beginning ++ replacement:((init center) ++ fin)
        
(^^^) :: Bits a => a -> a -> a
(^^^) = xor 
    
initializeInternalState :: Key -> IV -> State
initializeInternalState key initializationVector = foldState . fillInternalState key $ initializationVector

foldState :: State -> State
foldState state = foldl (\acc _ -> advanceState 177 288 t2 . advanceState 93 177 t1 . advanceState 0 93 t3 $ acc) state [1..(4 * 288)]

t1 = t_ 65  90  170
t2 = t_ 161 174 263
t3 = t_ 242 285 68

advanceState :: Int -> Int -> (State -> Bit) -> State -> State
advanceState from to tFunction state = shiftAndReplace (tFunction state) from to state 

t_ :: Int -> Int -> Int -> State -> Bit
t_ x y z state = (state !! x) ^^^ ((state !! y) .&. (state !! (y + 1))) ^^^ (state !! (y + 2)) ^^^ (state !! z)

nextState :: State -> (Bit, State)
nextState state =
    let f1 = (state !! 65)  ^^^ (state !! 92)
        f2 = (state !! 161) ^^^ (state !! 176)
        f3 = (state !! 242) ^^^ (state !! 287)

        key = f1 ^^^ f2 ^^^ f3

        f4 = f1 ^^^ ((state !! 90)  .&. (state !! 91))  ^^^ (state !! 170)
        f5 = f2 ^^^ ((state !! 174) .&. (state !! 175)) ^^^ (state !! 263)
        f6 = f3 ^^^ ((state !! 285) .&. (state !! 286)) ^^^ (state !! 68)

        firstReplacement = shiftAndReplace f6 0 93
        secondReplacement = shiftAndReplace f4 93 177
        thirdReplacement = shiftAndReplace f5 177 288
    in
        (key, thirdReplacement . secondReplacement . firstReplacement $ state)

nextByte :: State -> (Word8, State)
nextByte state =
    let (byte, finalState) = foldl (\(key, nextState) _ -> advanceByte key nextState) ([], state) [1..8]
    in
        (bitsToBytes byte, finalState)

advanceByte :: [Bit] -> State -> ([Bit], State)
advanceByte key state = 
    let (nextKey, ns) = nextState state
    in
        (key ++ [nextKey], ns)