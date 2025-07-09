-- HC6T1: Recursive factorial function
factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)

main1 :: IO ()
main1 = do
  putStrLn "HC6T1: Factorial of 5"
  print $ factorial 5  -- Expected: 120


-- HC6T2: Recursive Fibonacci function
fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)

main2 :: IO ()
main2 = do
  putStrLn "HC6T2: Fibonacci of 7"
  print $ fibonacci 7  -- Expected: 13


-- HC6T3: Sum using foldr
sumList :: [Int] -> Int
sumList = foldr (+) 0

main3 :: IO ()
main3 = do
  putStrLn "HC6T3: Sum of [1..10]"
  print $ sumList [1..10]  -- Expected: 55


-- HC6T4: Product using foldl
productList :: [Int] -> Int
productList = foldl (*) 1

main4 :: IO ()
main4 = do
  putStrLn "HC6T4: Product of [1..5]"
  print $ productList [1..5]  -- Expected: 120


-- HC6T5: Reverse a list using recursion
reverseList :: [a] -> [a]
reverseList [] = []
reverseList (x:xs) = reverseList xs ++ [x]

main5 :: IO ()
main5 = do
  putStrLn "HC6T5: Reverse of [1,2,3,4]"
  print $ reverseList [1,2,3,4]  -- Expected: [4,3,2,1]


-- HC6T6: Check if element exists in list
elementExists :: Eq a => a -> [a] -> Bool
elementExists _ [] = False
elementExists e (x:xs)
  | e == x    = True
  | otherwise = elementExists e xs

main6 :: IO ()
main6 = do
  putStrLn "HC6T6: Check if 3 exists in [1,2,3,4]"
  print $ elementExists 3 [1,2,3,4]  -- Expected: True


-- HC6T7: Length of a list
lengthOfList :: [a] -> Int
lengthOfList [] = 0
lengthOfList (_:xs) = 1 + lengthOfList xs

main7 :: IO ()
main7 = do
  putStrLn "HC6T7: Length of [\"a\", \"b\", \"c\"]"
  print $ lengthOfList ["a", "b", "c"]  -- Expected: 3


-- HC6T8: Filter even numbers
filterEvens :: [Int] -> [Int]
filterEvens = filter even

main8 :: IO ()
main8 = do
  putStrLn "HC6T8: Even numbers from [1..10]"
  print $ filterEvens [1..10]  -- Expected: [2,4,6,8,10]


-- HC6T9: Map implementation
myMap :: (a -> b) -> [a] -> [b]
myMap _ [] = []
myMap f (x:xs) = f x : myMap f xs

main9 :: IO ()
main9 = do
  putStrLn "HC6T9: myMap (*2) [1,2,3]"
  print $ myMap (*2) [1,2,3]  -- Expected: [2,4,6]


-- HC6T10: Digits of a number (recursive)
digits :: Int -> [Int]
digits n
  | n < 10    = [n]
  | otherwise = digits (n `div` 10) ++ [n `mod` 10]

main10 :: IO ()
main10 = do
  putStrLn "HC6T10: Digits of 12345"
  print $ digits 12345  -- Expected: [1,2,3,4,5]


-- Optional: Run all in one go
main :: IO ()
main = do
  main1
  main2
  main3
  main4
  main5
  main6
  main7
  main8
  main9
  main10
