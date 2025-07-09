{-# LANGUAGE RecordWildCards #-}

-- HC8T1: Type synonyms and transaction string
type Address = String
type Value = Int

generateTx :: Address -> Address -> Value -> String
generateTx from to val = "From: " ++ from ++ " -> To: " ++ to ++ " Amount: " ++ show val

main1 :: IO ()
main1 = do
  putStrLn "HC8T1:"
  putStrLn $ generateTx "Alice" "Bob" 100


-- HC8T2: PaymentMethod and Person type
data PaymentMethod = Cash | Card | Cryptocurrency deriving Show

data Person = Person
  { name :: String
  , address :: (String, Int)
  , payment :: PaymentMethod
  } deriving Show

bob :: Person
bob = Person "Bob" ("Main Street", 123) Cash

main2 :: IO ()
main2 = do
  putStrLn "HC8T2:"
  print bob


-- HC8T3: Shape and area function
data Shape = Circle Float | Rectangle Float Float

area :: Shape -> Float
area (Circle r) = pi * r * r
area (Rectangle w h) = w * h

main3 :: IO ()
main3 = do
  putStrLn "HC8T3:"
  print $ area (Circle 5)        -- 78.54...
  print $ area (Rectangle 10 5)  -- 50.0


-- HC8T4: Employee using record syntax
data Employee = Employee
  { employeeName :: String
  , experienceInYears :: Float
  } deriving Show

richard :: Employee
richard = Employee "Richard" 7.5

main4 :: IO ()
main4 = do
  putStrLn "HC8T4:"
  print richard


-- HC8T5: Person with employment status
data PersonInfo = PersonInfo
  { pName :: String
  , pAge :: Int
  , isEmployed :: Bool
  } deriving Show

person1 :: PersonInfo
person1 = PersonInfo "Alice" 30 True

person2 :: PersonInfo
person2 = PersonInfo "Charlie" 25 False

main5 :: IO ()
main5 = do
  putStrLn "HC8T5:"
  print person1
  print person2


-- HC8T6: Shape using record syntax
data ShapeRec
  = CircleRec { center :: (Float, Float), color :: String, radius :: Float }
  | RectangleRec { width :: Float, height :: Float, color :: String }
  deriving Show

circleShape :: ShapeRec
circleShape = CircleRec (0, 0) "Red" 10.0

rectangleShape :: ShapeRec
rectangleShape = RectangleRec 10.0 5.0 "Blue"

main6 :: IO ()
main6 = do
  putStrLn "HC8T6:"
  print circleShape
  print rectangleShape


-- HC8T7: Animal and describeAnimal
data Animal = Dog String | Cat String

describeAnimal :: Animal -> String
describeAnimal (Dog name) = "A dog named " ++ name
describeAnimal (Cat name) = "A cat named " ++ name

main7 :: IO ()
main7 = do
  putStrLn "HC8T7:"
  print $ describeAnimal (Dog "Rex")
  print $ describeAnimal (Cat "Whiskers")


-- HC8T8: Type synonym greet function
type Name = String
type Age = Int

greet :: Name -> Age -> String
greet name age = "Hello, my name is " ++ name ++ " and I am " ++ show age ++ " years old."

main8 :: IO ()
main8 = do
  putStrLn "HC8T8:"
  putStrLn $ greet "Diana" 28


-- HC8T9: Transaction type and creation
data Transaction = Transaction
  { from :: Address
  , to :: Address
  , amount :: Value
  , transactionId :: String
  } deriving Show

createTransaction :: Address -> Address -> Value -> String
createTransaction f t a = transactionId tx
  where tx = Transaction f t a ("TX-" ++ f ++ "-" ++ t ++ "-" ++ show a)

main9 :: IO ()
main9 = do
  putStrLn "HC8T9:"
  print $ createTransaction "Alice" "Bob" 150

