module Common where

import Data.Word
import qualified Data.ByteString as B
import Data.Bits

type Bit = Bool

parseToWord8 :: String -> [Word8]
parseToWord8 = map (toEnum . fromEnum)

readToWord8 :: String -> IO [Word8]
readToWord8 location = B.unpack <$> B.readFile location

toBits :: Word8 -> [Bit]
toBits x = [testBit x i | i <- [0.. finiteBitSize x - 1]]

bitsToBytes :: [Bit] -> Word8
bitsToBytes bits = foldl (\byte bit -> byte * 2 + if bit then 1 else 0) 0 bits