module Lib
    ( _N,_Z,_S,_P,_R,_M
    ) where

_N :: Int -> Int
_N _ = 0

_Z :: Int -> Int
_Z x = x + 1

_S :: ([Int] -> Int)  -> [[Int] -> Int] -> [Int] -> Int
_S g fs x = g (map (\f -> f x) fs)

_P :: Int -> [Int] -> Int
_P l x = x !! l

_R :: ([Int] -> Int) -> (Int -> Int -> [Int] -> Int) -> [Int] -> Int -> Int
_R f g xs y
  | y == 0 = f xs
  | y > 0 = g (y - 1) (_R f g xs (y - 1)) xs
  | otherwise = undefined

_M :: ([Int] -> Int) -> [Int] -> Int
_M f xs = (_Minimum f xs 0) where
  _Minimum :: ([Int] -> Int) -> [Int] -> Int -> Int
  _Minimum f xs i
     | (f (i : xs)) == 0 = i
     | otherwise = (_Minimum f xs (i + 1))
