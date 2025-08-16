module Main where

-- :: Integer -> Integer
-- means "this function takes an Integer and returns an Integer"
sumtorial :: Integer -> Integer
sumtorial 0 = 0
sumtorial n = n + sumtorial (n - 1)

hailstone :: Integer -> Integer
hailstone n
  | even n = n `div` 2
  | otherwise = 3 * n + 1

foo :: Integer -> Integer
foo 0 = 16
foo 1
  | "Haskell" > "C++" = 3
  | otherwise = 4
foo n
  | n < 0 = 0
  | n `mod` 17 == 2 = -43
  | otherwise = n + 3

f :: Int -> Int -> Int -> Int
f x y z = x + y + z

ex17 :: Int
ex17 = f 3 17 18

-- Lists
nums, range, range2 :: [Integer]
nums = [1, 2, 3, 19]
range = [1 .. 100]
range2 = [2, 4 .. 100]

-- String == [Char]
hello1 :: [Char]
hello1 = ['h', 'e', 'l', 'l', 'o']

hello2 :: String
hello2 = "hello"

helloSame :: Bool
helloSame = hello1 == hello2

hailstoneSeq :: Integer -> [Integer]
hailstoneSeq 1 = [1]
hailstoneSeq n = n : hailstoneSeq (hailstone n)

-- 이거 좀 어려운데 ㄷㄷ;
-- 실제로 이렇게 생각하는 법을 익혀야하는건가?
intListLength :: [Integer] -> Integer
intListLength [] = 0
-- _ is first element
-- xs is remaining
intListLength (_ : xs) = 1 + intListLength xs

sumEveryTwo :: [Integer] -> [Integer]
sumEveryTwo [] = []
sumEveryTwo [x] = [x]
sumEveryTwo (x : y : xs) = (x + y) : sumEveryTwo xs

-- Combining functions
-- 이게 비효율적이라고 느낄 수 있다. 왜냐면 전체 시퀀스를 생성하고 길이를 구하기 때문
hailstoneLen :: Integer -> Integer
hailstoneLen n = intListLength (hailstoneSeq n) - 1

main :: IO ()
main = putStrLn $ "The sumtorial of 5 is: " ++ show (sumtorial 5)