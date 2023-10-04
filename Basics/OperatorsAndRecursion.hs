import Prelude hiding ((&&), gcd)

infixr 3 &&

(&&) :: Bool -> Bool -> Bool
True && True = True
False && True = False
True && False = False
False && False = False

exOr :: Bool -> Bool -> Bool
exOr True a = not a
exOr False a = a

ifThenElse :: Bool -> Int -> Int -> Int
ifThenElse True a _ = a
ifThenElse False _ b = b

daysInMonth :: Int -> Int
daysInMonth x
    | x == 2 = 28
    | x == 4 || x == 6 || x == 9 || x == 11 = 30
    | otherwise = 31

validDate :: Int -> Int -> Bool
validDate x y = if x <= daysInMonth y then True else False

sumNumbers :: Int -> Int
sumNumbers 0 = 0
sumNumbers x = x + sumNumbers (x - 1)

sumSquares :: Int -> Int
sumSquares 0 = 0
sumSquares x = x^2 + sumSquares (x - 1)

power :: Int -> Int -> Int
power base 0 = 1
power base index
  | index == 1 = base
  | index > 0 = base * power base (index - 1)

sumFromTo :: Int -> Int -> Int
sumFromTo x y
    | x > y = 0
    | x == y = x
    | otherwise = x + sumFromTo (x + 1) y

gcd :: Int -> Int -> Int
gcd x y
  | x == y = x
  | otherwise = gcd (abs (x - y)) (min x y)

intSquareRoot :: Int -> Int
intSquareRoot n = findRoot n n

findRoot :: Int -> Int -> Int
findRoot x y
    | y^2 > x = findRoot x (y - 1)
    | otherwise = y
