-- CIS HW 1

    --Goal here is to make a function that takes a number, and splits it into a list containing the digits
toDigits :: Integer -> [Integer]
toDigits n
    | n <= 0    = []
    | otherwise = map (\c -> read [c]) $ show n
    -- Goal is same as above, but then reverse the digits 
toDigitsRev :: Integer -> [Integer]
toDigitsRev = reverse . toDigits

--Goal here is to double every second value, so second, fourth, etc.
      -- doubleEveryOtherRight is the final implementation
doubleEveryOtherRight :: [Integer] -> [Integer]
doubleEveryOtherRight n = reverse $ doubleEveryOther $ reverse n

         -- Helper funnction
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther []       = []
doubleEveryOther [x]      = [x]
doubleEveryOther (x:y:zs) = x : (2 * y) : doubleEveryOther zs


    --Goal here is to sum the *digits* present in the list

sumDigits :: [Integer] -> Integer
sumDigits []     = 0
sumDigits (x:xs) = (sum $ toDigits x) + sumDigits xs

   
validate :: Integer -> Bool
validate n = intermediate `mod` 10 == 0 
    where 
        intermediate =
        (sumDigits.doubleEveryOtherRight.toDigits) n
