-- CIS HW 1

    --Goal here is to make a function that takes a number, and splits it into a list containing the digits
toDigits :: Integer -> [Integer]
toDigits n
    | n <= 0    = []
    | otherwise = map (\c -> read [c]) $ show n
    -- Goal is same as above, but then reverse the digits 
toDigitsRev :: Integer -> [Integer]
toDigitsRev = reverse . toDigits

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther xs = 
 if even (length xs)
  then zipWith (\ x y -> (if odd  y then 2 * x else x)) xs [1..length xs]
  else zipWith (\ x y -> (if even y then 2 * x else x)) xs [1..length xs]


    --Goal here is to sum the *digits* present in the list

sumDigits :: [Integer] -> Integer
sumDigits []     = 0
sumDigits (x:xs) = (sum $ toDigits x) + sumDigits xs

   
validate :: Integer -> Bool
validate n = intermediate `mod` 10 == 0 
    where 
        intermediate =
        (sumDigits.doubleEveryOtherRight.toDigits) n
