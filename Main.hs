module Main (main) where

import Data.Char (chr)
import Data.List (foldl')

import qualified Online
import Decoder (makeDecoder, decode, receive)
import qualified Decoder
import Encoder (initEncoder, makeCheckBlocks)

makeData :: Int -> String
makeData bs = map f [0 .. dSize - 1]
  where
    f i = chr ((i `div` 64) `mod` 256)
    dSize = bs * Online.n

print' :: Show a => String -> a -> IO ()
print' s a = putStrLn $ s ++ ": " ++ show a

doOne :: Int -> IO ()
doOne s0 = do
    print' "seed" s0
    print' "bs" bs
    print' "nc" nc
    print' "solved?" $ Decoder.isDone dc2
    print' "d0 == d1" ok
  where
    bs = 16
    d0 = makeData bs
    ec = initEncoder d0
    nc = round $ fromIntegral Online.n * (1.4 :: Double)
    mbs = makeCheckBlocks ec nc s0

    -- decoder:
    dc0 = makeDecoder ()
    dc1 = foldl' (\dc (_, s, mb) -> receive dc s mb) dc0 mbs
    dc2 = decode dc1
    dc2D = Decoder.getData dc2
    ok = maybe False (== d0) dc2D

main :: IO ()
main = mapM_ doOne [100, 99 .. 1]
