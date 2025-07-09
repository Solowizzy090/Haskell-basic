{-# LANGUAGE FlexibleInstances #-}

-- HC7T1: Define Color and implement Eq
data Color = Red | Green | Blue deriving (Show, Read, Enum, Bounded)

instance Eq Color where
  Red   == Red   = True
  Green == Green = True
  Blue  == Blue  = True
  _     == _     = False

main1 :: IO ()
main1 = do
  putStrLn "HC7T1: Eq for Color"
  print (Red == Red)      -- True
  print (Red == Green)    -- False

-- HC7T2: Implement Ord for Color
instance Ord Color where
  compare Red Green   = LT
  compare Red Blue    = LT
  compare Green Blue  = LT
  compare a b
    | a == b    = EQ
    | otherwise = GT

main2 :: IO ()
main2 = do
  putStrLn "HC7T2: Ord for Color"
  print (Red < Green)     -- True
  print (Green < Blue)    -- True
  print (Blue > Red)      -- True

-- HC7T3: Function using Eq and Ord
compareValues :: (Eq a, Ord a) => a -> a -> a
compareValues a b = if a >= b then a else b

main3 :: IO ()
main3 = do
  putStrLn "HC7T3: compareValues"
  print $ compareValues 10 5       -- 10
  print $ compareValues "apple" "zebra"  -- "zebra"

-- HC7T4: Shape type with Show and Read
data Shape = Circle Double | Rectangle Double Double

instance Show Shape where
  show (Circle r) = "Circle " ++ show r
  show (Rectangle w h) = "Rectangle " ++ show w ++ " " ++ show h

instance Read Shape where
  readsPrec _ input =
    case words input of
      ["Circle", r]       -> [(Circle (read r), "")]
      ["Rectangle", w, h] -> [(Rectangle (read w) (read h), "")]
      _                   -> []

main4 :: IO ()
main4 = do
  putStrLn "HC7T4: Show and Read for Shape"
  let c = Circle 5.0
  let r = Rectangle 4.0 3.0
  print c
  print r
  print (read "Circle 5.0" :: Shape)

-- HC7T5: squareArea using Num
squareArea :: Num a => a -> a
squareArea side = side * side

main5 :: IO ()
main5 = do
  putStrLn "HC7T5: squareArea"
  print $ squareArea 4       -- 16
  print $ squareArea 3.5     -- 12.25

-- HC7T6: circleCircumference using Floating & Integral
circleCircumference :: (Real a, Floating b) => a -> b
circleCircumference r = 2 * pi * realToFrac r

main6 :: IO ()
main6 = do
  putStrLn "HC7T6: circleCircumference"
  print $ circleCircumference 5    -- ~31.4
  print $ circleCircumference (7 :: Integer)

-- HC7T7: nextColor using Enum and Bounded
nextColor :: Color -> Color
nextColor c
  | c == maxBound = minBound
  | otherwise     = succ c

main7 :: IO ()
main7 = do
  putStrLn "HC7T7: nextColor"
  print $ nextColor Red     -- Green
  print $ nextColor Green   -- Blue
  print $ nextColor Blue    -- Red (wrap around)

-- HC7T8: parseShape with Maybe
parseShape :: String -> Maybe Shape
parseShape str =
  case reads str of
    [(shape, "")] -> Just shape
    _             -> Nothing

main8 :: IO ()
main8 = do
  putStrLn "HC7T8: parseShape"
  print $ parseShape "Circle 10.0"          -- Just (Circle 10.0)
  print $ parseShape "Rectangle 5.0 3.0"    -- Just (Rectangle 5.0 3.0)
  print $ parseShape "Triangle 1.0 2.0"     -- Nothing

-- HC7T9: Describable type class
class Describable a where
  describe :: a -> String

instance Describable Bool where
  describe True  = "This is True"
  describe False = "This is False"

instance Describable Shape where
  describe (Circle r)        = "A circle with radius " ++ show r
  describe (Rectangle w h)   = "A rectangle with width " ++ show w ++ " and height " ++ show h

main9 :: IO ()
main9 = do
  putStrLn "HC7T9: Describable class"
  print $ describe True
  print $ describe (Circle 4.0)

-- HC7T10: describeAndCompare
describeAndCompare :: (Describable a, Ord a) => a -> a -> String
describeAndCompare a b = describe (max a b)

-- Making Shape an instance of Ord for test purposes
instance Eq Shape where
  (Circle r1) == (Circle r2) = r1 == r2
  (Rectangle w1 h1) == (Rectangle w2 h2) = w1 == w2 && h1 == h2
  _ == _ = False

instance Ord Shape where
  compare (Circle r1) (Circle r2) = compare r1 r2
  compare (Rectangle w1 h1) (Rectangle w2 h2) = compare (w1 * h1) (w2 * h2)
  compare (Circle r) (Rectangle w h) = compare (pi * r^2) (w * h)
  compare (Rectangle w h) (Circle r) = compare (w * h) (pi * r^2)

main10 :: IO ()
main10 = do
  putStrLn "HC7T10: describeAndCompare"
  let a = Circle 5
  let b = Rectangle 4 4
  print $ describeAndCompare a b
