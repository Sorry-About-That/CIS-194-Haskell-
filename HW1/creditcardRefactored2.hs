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
doubleEveryOther xs = zipWith (\x y -> if even y then 2*x else x) xs [size,size-1..1]
         where size = length xs

    --Goal here is to sum the *digits* present in the list

sumDigits :: [Integer] -> Integer
sumDigits []     = 0
sumDigits (x:xs) = (sum $ toDigits x) + sumDigits xs
   
validate :: Integer -> Bool
validate n = (sumDigits.doubleEveryOther.toDigits) n `mod` 10 == 0
