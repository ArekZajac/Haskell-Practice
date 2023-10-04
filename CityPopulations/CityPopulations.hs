import Data.List

type Location = (Float, Float)
type City = (String, Location, [Int])

type ScreenPosition = (Int,Int)

type Distances = (String, Float)

testData :: [City]
testData =
    [
        ("Amsterdam",(52,5),[1158,1149,1140,1132]),
        ("Athens",(38,23),[3153,3153,3154,3156]),
        ("Berlin",(53,13),[3567,3562,3557,3552]),
        ("Brussels",(51,4),[2096,2081,2065,2050]),
        ("Bucharest",(44,26),[1794,1803,1812,1821]),
        ("London",(52,0),[9426,9304,9177,9046]),
        ("Madrid",(40,4),[6669,6618,6559,6497]),
        ("Paris",(49,2),[11079,11017,10958,10901]),
        ("Rome",(42,13),[4278,4257,4234,4210]),
        ("Sofia",(43,23),[1284,1281,1277,1272]),
        ("Vienna",(48,16),[1945,1930,1915,1901]),
        ("Warsaw",(52,21),[1790,1783,1776,1768])
    ]

------ Q1

-- Returns the names of all the cities as a list of strings.
getNames :: [City] -> [String]
getNames = map $ \(name, _, _) -> name

------ Q2

-- Formats population figures in millions.
formatPopulation :: Int -> String
formatPopulation population = show (fromIntegral population / 1000) ++ "m"

-- Returns the population of a city a given number of years ago.
getPopulation :: [City] -> String -> Int -> String
getPopulation [] _ _ = "no data"
getPopulation (city:cities) toFind position
    | position >= length population = "no data"
    | toFind == name = formatPopulation (population !! position)
    | otherwise = getPopulation cities toFind position
    where
        (name, _, population) = city

------ Q3

-- Creates padding for text for better alignment.
pad :: Int -> Bool -> String -> String
pad count direction input
    -- True boolean for right pad, False boolean for left pad
    | not direction && count > length input = replicate (count - length input) ' ' ++ input
    | direction && count > length input = input ++ replicate (count - length input) ' '
    | otherwise = input

-- Prepares location values as strings with padding.
prepLocation :: Location -> String
prepLocation (long, lat) = pad 8 True (show long)  ++ pad 8 True (show lat)

-- Returns a well formatted string containing the 2 newest population values of a city.
newestPopulations :: [Int] -> String
newestPopulations [] = ""
newestPopulations popList = pad 10 True ( formatPopulation $ head popList) ++ pad 10 True ( formatPopulation $ popList!!1)

-- Returns a well formatted city entry for the table.
cityToString :: City -> String
cityToString (n, l, p) = "| " ++ pad 12 True n  ++ "| " ++ prepLocation l ++ "| " ++ newestPopulations p ++ "|"

-- Returns a well formatted table of all the cities.
citiesToTable :: [City] -> String
citiesToTable = unlines . map cityToString

------ Q4

-- Updates the list of cities with a new population value for each city.
updatePopulation :: [City] -> [Int] -> [City]
updatePopulation [] _ = []
updatePopulation (p:ps) (x:xs) = let (name, location, population) = p
    in (name, location, x : population) : updatePopulation ps xs

------ Q5

-- Updates the list of cities with a new city entry.
addCity :: [City] -> City -> [City]
addCity [] _ = []
-- sort function imported from Data.List
addCity cityList city = sort $ city : cityList

------ Q6

-- Calculates the change in percentage between two values.
percentageChange :: Int -> Int -> Float
percentageChange new original = ( (a-b) / b ) * 100
    where
        a = fromIntegral new :: Float
        b = fromIntegral original :: Float

-- Formats a float as a percentage string.
formatPercentage :: Float -> String
formatPercentage x = show x ++ "%"

-- Calculates the growth between two population values.
getGrowth :: Int -> Int -> String
getGrowth pop1 pop2 = formatPercentage $ percentageChange pop1 pop2

-- Returns the population growths between the population values of a given city.
getAllGrowth :: [City] -> String -> String
getAllGrowth (city:cities) toFind
    | toFind == name = getGrowth (head population) (population!!1) ++ " -> " ++ getGrowth (population!!1) (population!!2)++ " -> " ++ getGrowth (population!!2) (population!!3)
    | otherwise = getAllGrowth cities toFind
    where
        (name, _, population) = city

------ Q7

-- Calculates the euclidian distance wetween two location coordinates.
getDistance :: Location -> Location -> Float
getDistance (a1, a2) (b1, b2) = sqrt ((a1-b1)^2 + (a2-b2)^2)

-- Returns the location value from a city data type.
getLocation :: City -> Location
getLocation (_, l, _) = l

-- Returns the name value from a city data type.
getName :: City -> String
getName (n, _, _) = n

-- Returns a list of cities that have a greater population than the one provided.
allValidPopulations :: Int -> [City] -> [City]
allValidPopulations x = filter (\(_, _, pop) -> head pop > x)

-- Calculates all the distances between a location and all cities in a list.
allDistances :: Location -> [City] -> [Distances]
allDistances _ [] = []
allDistances loc (city:cities) = (name, dist) : allDistances loc cities
    where
        name = getName city
        dist = getDistance loc (getLocation city)

-- Returns the name value from a distances data type.
splitName :: Distances -> String
splitName (n,_) = n

-- Returns the name of the closest city with a population greater than a given number.
nearestValidCity :: [City] -> Location -> Int -> String
nearestValidCity cities loc minPop = splitName $ head $ sortOn snd distList
    where
        distList = allDistances loc (allValidPopulations minPop cities)

------ City Map

-- Removes all characters from the terminal.
clearScreen :: IO ()
clearScreen = putStr "\ESC[2J"

-- Changes the write position within the terminal to a given coordinate.
goTo :: ScreenPosition -> IO ()
goTo (x, y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

-- Prints given text to a certain position within the terminal.
writeAt :: ScreenPosition -> String -> IO ()
writeAt position text = do
    goTo position
    putStr text

-- Converts the location value of a city to a position of where it should be displayed on the screen.
locToPos :: Location -> ScreenPosition
locToPos (lat, long) = (round (long*2), round ((60-lat)*2))

-- Returns the most recent population value of a single city.
newestPopulation :: City -> String
newestPopulation (_, _, p) = formatPopulation $ head p

-- Prints a single city's name and population at a suitable position within the terminal.
drawCity :: City -> IO ()
drawCity city =
    do
        writeAt namePos ("+ " ++ getName city)
        writeAt (x, y+1) $ newestPopulation city
            where
                namePos = locToPos $ getLocation city
                (x, y) = namePos

-- Prints a map of all the city locations along with names and population values.
drawMap :: [City] -> IO ()
drawMap cities = clearScreen >> mapM_ drawCity cities >> goTo (0,50)

------ User Interface

-- Starts the UI for the application.
main :: IO ()
main = putStrLn "" >> drawSeperator 80 >> mainMenu testData

-- Shows the user the options for the application.
mainMenu :: [City] -> IO ()
mainMenu cities = do
    putStrLn ""
    putStrLn "Enter a number corresponding to the following options:"
    putStrLn "[1] - List all cities."
    putStrLn "[2] - Show the population of a city."
    putStrLn "[3] - List all cities along with their most recent population values."
    putStrLn "[4] - Update all cities with new population values."
    putStrLn "[5] - Add a new city to the city list."
    putStrLn "[6] - Show population growths for a given city."
    putStrLn "[7] - Show the closest city with a certain population."
    putStrLn "[8] - Draw a map of the cities."
    putStrLn "[0] - Exit application."
    putStrLn ""
    putStr "> "
    choice <- getLine
    putStrLn ""
    option choice cities

-- A mini-menu that acts as a transition between an option and the main menu.
returnMenu :: [City] -> IO ()
returnMenu cities = do
    putStrLn ""
    putStr "Press [Enter] key to return to menu"
    _ <- getLine
    clearScreen
    drawSeperator 80
    mainMenu cities

-- Draws a line accross the screen to act as visual separation.
drawSeperator :: Int -> IO ()
drawSeperator 0 =
    do
        putStrLn ""
        return ()
drawSeperator x =
    do
        putStr "-"
        drawSeperator (x-1)

-- Prints the contents of a text file.
readFromFile :: IO ()
readFromFile = do
    let file = "cityData.txt"
    contents <- readFile file
    putStrLn contents

-- Calls the part of the application corresponding to the user's option within the main menu.
option :: String -> [City] -> IO ()
option "1" cities = do
    print (getNames cities)
    returnMenu cities
option "2" cities = do
    putStrLn "Enter a city name:" >> putStr "> "
    inputName <- getLine
    putStrLn ""
    putStrLn "Population from how many years ago?" >> putStr "> "
    inputYear <- getLine
    putStrLn ""
    print (getPopulation testData inputName (read inputYear))
    returnMenu cities
option "3" cities = do
    putStrLn (citiesToTable testData)
    returnMenu cities
option "4" cities = do
    putStrLn "Enter new population values as a list of integers:" >> putStr "> "
    input <- getLine
    putStrLn ""
    putStrLn (citiesToTable (updatePopulation testData (read input)))
    returnMenu cities
option "5" cities = do
    putStrLn "Enter a new city to add to the list" >> putStr "> "
    input <- getLine
    putStrLn ""
    putStrLn input
    returnMenu cities
option "6" cities = do
    putStrLn "Enter a city name:" >> putStr "> "
    input <- getLine
    putStrLn ""
    print (getAllGrowth testData input)
    returnMenu cities
option "7" cities = do
    putStrLn "Enter a location:" >> putStr "> "
    inputLocation <- getLine
    putStrLn ""
    putStrLn "Enter a minimum population in thousands:" >> putStr "> "
    inputPopulation <- getLine
    putStrLn ""
    putStr (nearestValidCity testData (read inputLocation) (read inputPopulation))
    putStrLn (" Is the closest city to " ++ inputLocation ++ " with at least " ++ inputPopulation ++ " thousand people.")
    returnMenu cities
option "8" cities = do
    drawMap cities
    returnMenu cities
option "0" cities = do
    clearScreen
    return ()
option _ cities = do
    putStrLn "Invalid selection"
    returnMenu cities

------ Demos

demo :: Int -> IO ()
demo 1 = print (getNames testData)
demo 2 = print (getPopulation testData "Madrid" 2)
demo 3 = putStrLn (citiesToTable testData)
demo 4 = putStrLn (citiesToTable (updatePopulation testData [1200,3200,3600,2100,1800,9500,6700,11100,4300,1300,2000,1800]))
demo 5 = putStrLn (citiesToTable (addCity testData ("Prague",(50,14),[1312,1306,1299,1292])))
demo 6 = print (getAllGrowth testData "London")
demo 7 = print (nearestValidCity testData (54,6) 2000)
demo 8 = drawMap testData