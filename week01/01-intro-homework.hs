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