-- CIS HW 1

    --Goal here is to make a function that takes a number, and splits it into a list containing the digits
toDigits :: Integer -> [Integer]
toDigits n
    | n <= 0 = []
    | otherwise = toDigits (n `div` 10) ++ [n `mod` 10]
    -- Goal is same as above, but then reverse the digits 
toDigitsRev :: Integer -> [Integer]
toDigitsRev n
    | n <= 0 = []
    | otherwise = reverse (toDigits n)

    --Goal here is to double every second value, so second, fourth, etc.
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther [x] = [x]
doubleEveryOther (x:y:zs) = x : (2 * y) : doubleEveryOther zs

doubleEveryOtherRight :: [Integer] -> [Integer]
doubleEveryOtherRight n = reverse (doubleEveryOther (reverse n))
  -- doubleEveryOtherRight is the final implementation


    --Goal here is to sum the *digits* present in the list

singleSum :: [Integer] -> Integer
singleSum xs = sum [x | x <- xs, x < 10]

doubleSum :: [Integer] -> Integer
doubleSum xs = sum (map sum (map toDigits (doubleNumbers xs)))
    where doubleNumbers xs = [x | x <- xs, x >= 10]

  -- sumDigits is the final implementation
sumDigits :: [Integer] -> Integer
sumDigits xs = singleSum xs + doubleSum xs

   
validate :: Integer -> Bool
validate n = remainderTen == 0 
    where 
        remainderTen = intermediate `mod` 10 
        intermediate = sumDigits (doubleEveryOtherRight (toDigits n))
