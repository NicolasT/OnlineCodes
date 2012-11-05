
module MB(
  MB,
  xorMB,
  zeroMB,
  splitInMBs,
  combineMBs,
  ) 
       where

import Data.Bits(xor)
import Data.Char(chr,ord)

import Rhs

import Data.Word (Word8)

import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as UV



newtype MB = MB (Vector Word8)
unMB :: MB -> Vector Word8
unMB (MB v) = v

zeroMB :: Int -> MB
zeroMB n = MB (UV.replicate n 0)

splitInMBs :: Int -> String -> [MB]
splitInMBs bs = loop []
  where
    loop acc s | null s = reverse acc
               | otherwise = loop (MB (UV.fromList $ map (fromIntegral . ord) h) : acc) t
      where
        (h, t) = splitAt bs s


combineMBs :: [MB] -> Int -> String
combineMBs mbs n = concatMap (map (chr . fromIntegral) . UV.toList . unMB) $ take n mbs

instance Show MB where
  show (MB v) = "MB <" ++ show (UV.length v) ++ " * '" ++ show (UV.head v) ++ "'>"
  
instance RhsC MB where
  (-:) = xorMB

xorMB :: MB -> MB -> MB
xorMB (MB va) (MB vb) = MB $ UV.zipWith xor va vb
