-- HC10T1: ShowSimple type class and instance for PaymentMethod
data PaymentMethod = Cash | Card | Cryptocurrency deriving (Show)

class ShowSimple a where
  showSimple :: a -> String

instance ShowSimple PaymentMethod where
  showSimple Cash = "Cash"
  showSimple Card = "Card"
  showSimple Cryptocurrency = "Crypto"

main1 :: IO ()
main1 = do
  putStrLn "HC10T1:"
  print $ showSimple Cash
  print $ showSimple Card
  print $ showSimple Cryptocurrency


-- HC10T2: Summable type class and instance for Int
class Summable a where
  sumUp :: [a] -> a

instance Summable Int where
  sumUp = sum

main2 :: IO ()
main2 = do
  putStrLn "HC10T2:"
  print $ sumUp [1,2,3,4 :: Int]


-- HC10T3: Comparable for Blockchain
data Blockchain = BTC | ETH | SOL deriving (Show)

class Comparable a where
  compareWith :: a -> a -> Ordering

instance Comparable Blockchain where
  compareWith BTC BTC = EQ
  compareWith BTC _   = LT
  compareWith ETH BTC = GT
  compareWith ETH ETH = EQ
  compareWith ETH _   = LT
  compareWith SOL SOL = EQ
  compareWith SOL _   = GT

main3 :: IO ()
main3 = do
  putStrLn "HC10T3:"
  print $ compareWith BTC ETH
  print $ compareWith SOL ETH
  print $ compareWith BTC BTC


-- HC10T4: Eq instance for Box
data Box a = Empty | Has a deriving Show

instance Eq a => Eq (Box a) where
  Empty == Empty = True
  Has x == Has y = x == y
  _ == _ = False

main4 :: IO ()
main4 = do
  putStrLn "HC10T4:"
  print $ Has 5 == Has 5
  print $ Empty == Empty
  print $ Has 1 == Empty


-- HC10T5: ShowDetailed type class and User instance
data User = User { username :: String, age :: Int }

class ShowDetailed a where
  showDetailed :: a -> String

instance ShowDetailed User where
  showDetailed (User u a) = "Username: " ++ u ++ ", Age: " ++ show a

main5 :: IO ()
main5 = do
  putStrLn "HC10T5:"
  let u = User "alice" 30
  putStrLn $ showDetailed u


-- HC10T6: Mutual recursion in Eq for Blockchain
instance Eq Blockchain where
  x == y = not (x /= y)
  x /= y = not (x == y)

main6 :: IO ()
main6 = do
  putStrLn "HC10T6:"
  print $ BTC == BTC
  print $ ETH /= SOL


-- HC10T7: Convertible type class and PaymentMethod -> String
class Convertible a b where
  convert :: a -> b

instance Convertible PaymentMethod String where
  convert Cash = "Paying by cash"
  convert Card = "Paying by card"
  convert Cryptocurrency = "Paying by crypto"

main7 :: IO ()
main7 = do
  putStrLn "HC10T7:"
  print $ convert Card
  print $ convert Cryptocurrency


-- HC10T8: AdvancedEq subclass of Eq
class Eq a => AdvancedEq a where
  compareEquality :: a -> a -> Bool
  compareEquality x y = x == y

instance AdvancedEq Int

main8 :: IO ()
main8 = do
  putStrLn "HC10T8:"
  print $ compareEquality (5 :: Int) 5
  print $ compareEquality (3 :: Int) 10


-- HC10T9: MinMax type class for Int
class MinMax a where
  minValue :: a
  maxValue :: a

instance MinMax Int where
  minValue = minBound
  maxValue = maxBound

main9 :: IO ()
main9 = do
  putStrLn "HC10T9:"
  print (minValue :: Int)
  print (maxValue :: Int)


-- HC10T10: Concatenatable type class for [Char]
class Concatenatable a where
  concatWith :: a -> a -> a

instance Concatenatable [Char] where
  concatWith = (++)

main10 :: IO ()
main10 = do
  putStrLn "HC10T10:"
  print $ concatWith "Hello, " "world!"


-- Combined main for testing all
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
