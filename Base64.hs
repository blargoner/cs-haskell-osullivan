module Base64 (
    encode
)
where

import Data.Char
import Data.Word
import Data.Bits

b64 :: String
b64 = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"

encode' :: String -> String
encode' [] = []
encode' (x:y:z:xs) = a:b:c:d:encode' xs
    where k = fromIntegral $ ord x :: Word8
          l = fromIntegral $ ord y :: Word8
          m = fromIntegral $ ord z :: Word8

          p = fromIntegral $ shiftR k 2 :: Int
          q = fromIntegral $ (shiftL (k .&. 0x3) 4) .|. (shiftR l 4) :: Int
          r = fromIntegral $ (shiftL (l .&. 0xf) 2) .|. (shiftR m 6) :: Int
          s = fromIntegral $ (m .&. 0x3f) :: Int

          a = b64 !! p
          b = b64 !! q
          c = b64 !! r
          d = b64 !! s

encode :: String -> String
encode s = e
    where p = (3 - (length s `mod` 3)) `mod` 3
          s' = s ++ replicate p (chr 0)
          e' = encode' s'
          e = take (length e' - p) e' ++ replicate p '='
