-- https://www.seas.upenn.edu/~cis1940/spring13/hw/01-intro.pdf
-- Example 1: 숫자 → 자릿수 리스트
toDigits :: Integer -> [Integer]
toDigits n
  | n <= 0 = []
  | otherwise = toDigits (n `div` 10) ++ [n `mod` 10]

rev :: [Integer] -> [Integer]
rev [] = []
rev (x : xs) = rev xs ++ [x]

toDigitsRev :: Integer -> [Integer]
toDigitsRev n
  | n <= 0 = []
  | otherwise = rev (toDigits n)

-- Example 2: 왼쪽에서 2번째, 4번째...를 두 배(= 뒤집힌 리스트 기준)
doubleEveryEven :: [Integer] -> [Integer]
doubleEveryEven [] = []
doubleEveryEven (y1 : []) = [y1]
doubleEveryEven (y1 : y2 : ys) = y1 : 2 * y2 : doubleEveryEven ys

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther xs = rev (doubleEveryEven (rev xs))

-- Example 3: 자릿수 합 (리스트 전체)
sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x : xs) = sumDigits (toDigits x) + sumDigits xs

-- Example 4: Luhn 검증
validate :: Integer -> Bool
validate n = sumDigits (doubleEveryOther (toDigits n)) `mod` 10 == 0

-- Example 5: 하노이탑
type Peg = String

type Move = (Peg, Peg)

-- 1. move (n-1) discs from 'a' to 'c'
-- 2. move the top disc from 'a' to 'b'
-- 3. move (n-1) discs from 'c' to 'b'
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi 1 a b _ = [(a, b)]
hanoi num a b c = hanoi (num - 1) a c b ++ [(a, b)] ++ hanoi (num - 1) c b a
