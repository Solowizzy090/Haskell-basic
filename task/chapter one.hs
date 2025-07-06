-- HC1T1 - Task 1: Function Composition
double :: Int -> Int
double x = x * 2

increment :: Int -> Int
increment x = x + 1

doubleThenIncrement :: Int -> Int
doubleThenIncrement = increment . double

main1 :: IO ()
main1 = print (doubleThenIncrement 4) -- Should print 9


-- HC1T2 - Task 2: Pure Function Example
circleArea :: Floating a => a -> a
circleArea r = pi * r * r

main2 :: IO ()
main2 = print (circleArea 5) -- Should print 78.53981633974483


-- HC1T3 - Task 3: Checking if a Number is Greater than 18
greaterThan18 :: Int -> Bool
greaterThan18 x = x > 18

main3 :: IO ()
main3 = print (greaterThan18 20) -- Should print True


-- HC1T4 - Task 4: Composing a Function to Process Player Data
type Player = (String, Int)

extractPlayers :: [Player] -> [String]
extractPlayers = map fst

sortByScore :: [Player] -> [Player]
sortByScore = reverse . quicksort
  where
    quicksort [] = []
    quicksort (x:xs) = quicksort [y | y <- xs, snd y <= snd x] 
                       ++ [x] ++ 
                       quicksort [y | y <- xs, snd y > snd x]

topThree :: [Player] -> [Player]
topThree = take 3

getTopThreePlayers :: [Player] -> [String]
getTopThreePlayers = extractPlayers . topThree . sortByScore

main4 :: IO ()
main4 = print (getTopThreePlayers [("Alice", 90), ("Bob", 85), ("Eve", 92), ("Dan", 88)]) 
-- Should print ["Eve","Alice","Dan"]


-- HC1T5 - Task 5: Laziness in Haskell
infiniteNumbers :: [Integer]
infiniteNumbers = [1..]

takeN :: Int -> [Integer]
takeN n = take n infiniteNumbers

main5 :: IO ()
main5 = print (takeN 10) -- Should print [1,2,3,4,5,6,7,8,9,10]


-- HC1T6 - Task 6: Using Type Signatures
addNumbers :: Int -> Int -> Int
addNumbers x y = x + y

main6 :: IO ()
main6 = print (addNumbers 10 15) -- Should print 25


-- HC1T7 - Task 7: Converting Fahrenheit to Celsius
fToC :: Fractional a => a -> a
fToC f = (f - 32) * 5 / 9

main7 :: IO ()
main7 = print (fToC 98.6) -- Should print 37.0


-- HC1T8 - Task 8: Higher-Order Functions
applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

main8 :: IO ()
main8 = print (applyTwice (+3) 4) -- Should print 10
