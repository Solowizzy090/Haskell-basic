-- HC5T1 to HC5T10 Combined Implementation

-- Task 1: Apply a function three times to an integer
applyThrice :: (Int -> Int) -> Int -> Int
applyThrice f x = f (f (f x))

-- Task 2: Filter odd numbers from 1 to 30
oddNumbers :: [Int]
oddNumbers = filter odd [1..30]

-- Task 3: Check if any word starts with an uppercase letter
hasUppercaseStart :: [String] -> Bool
hasUppercaseStart = any (\word -> not (null word) && head word `elem` ['A'..'Z'])

-- Task 4: Rewrite using lambda
biggerThan10 :: Int -> Bool
biggerThan10 = \x -> x > 10

-- Task 5: Partial application to multiply by 5
multiplyByFive :: Int -> Int
multiplyByFive = (*) 5

-- Task 6: Function composition to square and filter even results
squareEvens :: [Int] -> [Int]
squareEvens = filter even . map (^2)

-- Task 7: Rewrite using the $ operator
result :: Int
result = sum $ map (*2) $ filter (>3) [1..10]

-- Task 8: Point-free style
addFive :: Int -> Int
addFive = (+5)

-- Task 9: Apply a function twice to every element in a list
transformList :: (a -> a) -> [a] -> [a]
transformList f = map (f . f)

-- Task 10: Combine filter, map, and any to check if any squared value is > 50
anySquareGreaterThan50 :: [Int] -> Bool
anySquareGreaterThan50 = any (>50) . map (^2) . filter (>0)

-- Main for demonstration
main :: IO ()
main = do
    putStrLn "-- HC5T1: applyThrice (+1) to 5 --"
    print $ applyThrice (+1) 5  -- Expected: 8

    putStrLn "\n-- HC5T2: oddNumbers from 1 to 30 --"
    print oddNumbers

    putStrLn "\n-- HC5T3: hasUppercaseStart [\"hello\", \"World\"] --"
    print $ hasUppercaseStart ["hello", "World"]  -- True

    putStrLn "\n-- HC5T4: biggerThan10 15 --"
    print $ biggerThan10 15

    putStrLn "\n-- HC5T5: multiplyByFive 6 --"
    print $ multiplyByFive 6  -- Expected: 30

    putStrLn "\n-- HC5T6: squareEvens [1..10] --"
    print $ squareEvens [1..10]  -- Expected: even squares

    putStrLn "\n-- HC5T7: result = sum $ map (*2) $ filter (>3) [1..10] --"
    print result  -- Should match sum (map (*2) (filter (>3) [1..10]))

    putStrLn "\n-- HC5T8: addFive 7 --"
    print $ addFive 7  -- Expected: 12

    putStrLn "\n-- HC5T9: transformList (*2) [1,2,3] --"
    print $ transformList (*2) [1,2,3]  -- Expected: [4,8,12]

    putStrLn "\n-- HC5T10: anySquareGreaterThan50 [5,6,7] --"
    print $ anySquareGreaterThan50 [5,6,7]  -- Expected: True (49)
