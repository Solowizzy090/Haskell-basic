-- HC4T1 to HC4T8 Combined Implementation

-- Task 1: weatherReport
weatherReport :: String -> String
weatherReport "sunny"  = "It's a bright and beautiful day!"
weatherReport "rainy"  = "Don't forget your umbrella!"
weatherReport "cloudy" = "A bit gloomy, but no rain yet!"
weatherReport _        = "Weather unknown"

-- Task 2: dayType
dayType :: String -> String
dayType "Saturday" = "It's a weekend!"
dayType "Sunday"   = "It's a weekend!"
dayType day
    | day `elem` ["Monday", "Tuesday", "Wednesday", "Thursday", "Friday"] = "It's a weekday."
    | otherwise = "Invalid day"

-- Task 3: gradeComment
gradeComment :: Int -> String
gradeComment grade
    | grade >= 90 && grade <= 100 = "Excellent!"
    | grade >= 70 && grade <= 89  = "Good job!"
    | grade >= 50 && grade <= 69  = "You passed."
    | grade >= 0  && grade <= 49  = "Better luck next time."
    | otherwise                   = "Invalid grade"

-- Task 4 & 5: specialBirthday (pattern matching + catch-all)
specialBirthday :: Int -> String
specialBirthday 1  = "Happy 1st Birthday!"
specialBirthday 18 = "Congrats on becoming an adult!"
specialBirthday 50 = "Happy 50th Birthday!"
specialBirthday age = "Happy Birthday! You are " ++ show age ++ " years old!"

-- Task 6: whatsInsideThisList
whatsInsideThisList :: [a] -> String
whatsInsideThisList []      = "The list is empty."
whatsInsideThisList [_]     = "The list has one element."
whatsInsideThisList [_, _]  = "The list has two elements."
whatsInsideThisList _       = "The list has many elements."

-- Task 7: firstAndThird
firstAndThird :: [a] -> [a]
firstAndThird (x:_:z:_) = [x, z]
firstAndThird _         = []

-- Task 8: describeTuple
describeTuple :: (String, Int) -> String
describeTuple (name, age) = name ++ " is " ++ show age ++ " years old."

-- Main function to test everything
main :: IO ()
main = do
    putStrLn "-- Task 1: weatherReport --"
    print $ weatherReport "sunny"
    print $ weatherReport "foggy"

    putStrLn "\n-- Task 2: dayType --"
    print $ dayType "Sunday"
    print $ dayType "Wednesday"
    print $ dayType "Funday"

    putStrLn "\n-- Task 3: gradeComment --"
    print $ gradeComment 95
    print $ gradeComment 72
    print $ gradeComment 40
    print $ gradeComment 150

    putStrLn "\n-- Task 4 & 5: specialBirthday --"
    print $ specialBirthday 1
    print $ specialBirthday 18
    print $ specialBirthday 30

    putStrLn "\n-- Task 6: whatsInsideThisList --"
    print $ whatsInsideThisList ([] :: [Int])
    print $ whatsInsideThisList [1]
    print $ whatsInsideThisList [1,2]
    print $ whatsInsideThisList [1,2,3]

    putStrLn "\n-- Task 7: firstAndThird --"
    print $ firstAndThird [10,20,30,40]
    print $ firstAndThird [1,2]
    
    putStrLn "\n-- Task 8: describeTuple --"
    print $ describeTuple ("Alice", 30)
    print $ describeTuple ("Bob", 45)
