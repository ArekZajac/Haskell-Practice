import Data.Char
type StudentMark = (String, Int)

sumDifference :: Int -> Int -> (Int, Int)
sumDifference x y = (x + y , x - y)

grade :: StudentMark -> Char
grade (x,y)
    | y >= 70 = 'A'
    | y >= 60 = 'B'
    | y >= 50 = 'C'
    | y >= 40 = 'D'
    | otherwise = 'F'

capMark :: StudentMark -> StudentMark
capMark (x,y)
    | y > 40 = (x,40)
    | otherwise = (x,y)

firstNumbers :: Int -> [Int]
firstNumbers x = [0..x]

firstSquares :: Int -> [Int]
firstSquares x = [y^2 | y <- [1..x]]

capitalise :: String -> String
capitalise x = map toUpper x

onlyDigits :: String -> String
onlyDigits x = filter (`elem` ['0'..'9']) x

capMarks :: [StudentMark] -> [StudentMark]
capMarks stdl = [capMark std | std <- stdl]

gradeStudents :: [StudentMark] -> [(String,Char)]
gradeStudents stdl = [(std, grade (std, mrk)) | (std, mrk) <- stdl]

duplicate :: String -> Int -> String
duplicate x y = concat $ replicate y x

divisors :: Int -> [Int]
divisors n = [x | x <- [1..(n-1)], n `rem` x == 0]

isPrime :: Int -> Bool
isPrime x
    | length(divisors x) == 2 = True
    | otherwise = False

split :: [(a,b)] -> ([a],[b])
split lst = ([a | (a,x) <- lst], [b | (x,b) <- lst])
