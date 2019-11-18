module Common(
    Bit,
    Buffer,
    parseToWord8,
    readToWord8,
    writeWord8,
    toBits,
    bitsToBytes,
    readHexString,
    fromHex,
    rotate,
    splitAt2,
    mapFst,
    mapSnd,
    (+:)
) where

import Data.Word
import qualified Data.ByteString as B
import Data.Bits (testBit, finiteBitSize)
import Numeric
import Data.List.Split
import Data.ByteString.UTF8 (fromString)

type Bit = Bool

parseToWord8 :: String -> [Word8]
parseToWord8 = map (toEnum . fromEnum)

readToWord8 :: String -> IO [Word8]
readToWord8 location = B.unpack <$> B.readFile location

writeWord8 :: String -> [Word8] -> IO ()
writeWord8 location bytes = B.writeFile location . B.pack $ bytes

toBits :: Word8 -> [Bit]
toBits x = [testBit x i | i <- [0.. finiteBitSize x - 1]]

bitsToBytes :: [Bit] -> Word8
bitsToBytes bits = foldl (\byte bit -> byte * 2 + if bit then 1 else 0) 0 bits

type Buffer = [Word8]

readHexString :: String -> [Word8]
readHexString str = map (fst . head . readHex) . chunksOf 2 $ str

fromHex :: [Word8] -> String
fromHex = concatMap (\x -> showHex x "")

rotate :: Int -> [a] -> [a]
rotate _ [] = []
rotate n xs = zipWith const (drop n (cycle xs)) xs

splitAt2 :: Int -> Int -> [a] -> ([a], [a], [a])
splitAt2 firstPosition secondPosition list =
    let (xs, tail) = splitAt firstPosition list
        (ys, zs) = splitAt (secondPosition - firstPosition) tail
    in
        (xs, ys, zs)

mapFst :: (a -> c) -> (a, b) -> (c, b)
mapFst f (a, b) = (f a, b)

mapSnd :: (b -> c) -> (a, b) -> (a, c)
mapSnd f (a, b) = (a, f b)

(+:) :: a -> [a] -> [a]
(+:) x xs = xs ++ [x]