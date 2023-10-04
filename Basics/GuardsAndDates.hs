absolute :: Int -> Int
absolute x
  | x > 0 = x
  | otherwise = - x

sign :: Int -> Int
sign x
  | x > 0 = 1
  | x < 0 = -1
  | otherwise = 0

howManyEqual :: Int -> Int -> Int -> Int
howManyEqual x y z
  | x == y && x == z = 3
  | x /= y && x /= z && y /= z = 0
  | otherwise = 2

sumDiagonalLength :: Float -> Float -> Float -> Float
sumDiagonalLength x y z = diag x + diag y + diag z
  where
    diag a = sqrt (2 * a ^ 2)

taxiFare :: Int -> Float
taxiFare x = 2.20 + rest x
  where
    rest x
      | x < 10 = fromIntegral x * 0.5
      | otherwise = fromIntegral (x - 10) * 0.3 + 5

howManyAboveAverage :: Int -> Int -> Int -> Int
howManyAboveAverage x y z
  | fromIntegral x > avg && fromIntegral y > avg = 2
  | fromIntegral x > avg && fromIntegral z > avg = 2
  | fromIntegral y > avg && fromIntegral z > avg = 2
  | x == y && x == z = 0
  | otherwise = 1
  where
    avg = fromIntegral (x + y + z) / 3

validDate :: Int -> Int -> Bool
validDate d m
  | d > 0 && d <= 30 && monthShort = True
  | d > 0 && d <= 28 && m == 2 = True
  | d > 0 && d <= 31 && not (monthShort || m == 2) = True
  | otherwise = False
  where
    monthShort = m == 4 || m == 6 || m == 9 || m == 11

daysInMonth :: Int -> Int -> Int
daysInMonth m y
  | m == 2 && mod y 4 == 0 = 29
  | m == 2 = 28
  | m == 4 || m == 6 || m == 9 || m == 11 = 30
  | otherwise = 31
