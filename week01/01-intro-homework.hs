{-# OPTIONS_GHC -Wall #-}
-- CIS 194: Homework 1
-- ===================

-- Exercise 1:
-- toDigits should convert positive Integers to a list of digits.
-- (For 0 or negative inputs, toDigits should return the empty list.)
-- toDigitsRev should do the same, but with the digits reversed.


toDigits :: Integer -> [Integer]
toDigits x
  | x <= 0 = []
  | otherwise = toDigits (x `div` 10) ++ [x `mod` 10]

toDigitsRev :: Integer -> [Integer]
toDigitsRev x
  | x <= 0 = []
  | otherwise = [x `mod` 10] ++ toDigitsRev (x `div` 10)

-- Exercise 2:
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther xs
    | xs == [] = []
    | isEvenLenghtList = zipWith (*) (cycle [2,1]) xs
    | otherwise = zipWith (*) (cycle [1,2]) xs
    where isEvenLenghtList = length xs `mod` 2 == 0

-- Exercise 3:
toDigitList :: [Integer] -> [Integer]
toDigitList [] = []
toDigitList [x] = toDigits x
toDigitList (x:xs) = toDigits x ++ toDigitList(xs)

sumDigits :: [Integer] -> Integer
sumDigits = sum . toDigitList


-- Exercise 4 Define the function
checkSum :: Integer -> Bool
checkSum x = x `mod` 10  == 0

validate :: Integer -> Bool
validate = checkSum . sumDigits . doubleEveryOther . toDigits

-- Exercise 5 The Towers of Hanoi

type Peg = String
type Move = (Peg, Peg)

--1. move n − 1 discs from a to c using b as temporary storage
--2. move the top disc from a to b
--3. move n − 1 discs from c to b using a as temporary storage.
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n a b c
  | n < 1 = []
  | n == 1 = [(a, b)]
  | otherwise = (hanoi (n -1) a c b) ++ (hanoi 1 a b c) ++ (hanoi (n -1) c b a)


--1. move (aprr.) first half of the discs from a to c using b and d as temporary storage
--2. move (aprr.) second half of the discs from a to d using b and c as temporary storage
--3. move the top disc from a to b
--4. move (aprr.) second half of the discs from d to b using a nd c as temporary storage
--4. move (aprr.) first half of the discs from c to b using  a and b as temporary storage
hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4 n a b c d
  | n < 1 = []
  | n == 1 = [(a, b)]
  | otherwise =
       (hanoi4 firstHalf a c b d)
    ++ (hanoi4 secondHalf a d b c)
    ++ (hanoi4 1 a b c d)
    ++ (hanoi4 secondHalf d b a c)
    ++ (hanoi4 firstHalf c b a d)
    where
      disksToMove = n - 1
      firstHalf = disksToMove `div` 2
      secondHalf = disksToMove - firstHalf

hanoiMoveCount :: Integer -> Peg -> Peg -> Peg -> Int
hanoiMoveCount n a b c = length (hanoi n a b c)

hanoi4MoveCount :: Integer -> Peg -> Peg -> Peg -> Peg -> Int
hanoi4MoveCount n a b c d = length (hanoi4 n a b c d)
