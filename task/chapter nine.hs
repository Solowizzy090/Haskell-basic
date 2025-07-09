-- HC9T1: Define a Parametric Type Synonym
type Entity a = (a, String)  -- (address type, entity name)

main1 :: IO ()
main1 = do
  putStrLn "HC9T1:"
  let e1 :: Entity Int = (12345, "Warehouse")
  let e2 :: Entity String = ("0xABC", "SmartContract")
  print e1
  print e2


-- HC9T2: Implement a Parametric Data Type
data Box a = Empty | Has a deriving Show

main2 :: IO ()
main2 = do
  putStrLn "HC9T2:"
  print (Has 5)
  print (Empty :: Box Int)


-- HC9T3: Define a Function to Add Values in a Box
addN :: Num a => a -> Box a -> Box a
addN n (Has x) = Has (n + x)
addN _ Empty = Empty

main3 :: IO ()
main3 = do
  putStrLn "HC9T3:"
  print $ addN 3 (Has 7)     -- Has 10
  print $ addN 10 Empty      -- Empty


-- HC9T4: Extract a Value from a Box
extract :: a -> Box a -> a
extract _ (Has x) = x
extract def Empty = def

main4 :: IO ()
main4 = do
  putStrLn "HC9T4:"
  print $ extract 0 (Has 42)    -- 42
  print $ extract 100 Empty     -- 100


-- HC9T5: Parametric Shape with Record Syntax
data Shape a
  = Circle { color :: a, radius :: Float }
  | Rectangle { color :: a, width :: Float, height :: Float }
  deriving Show

main5 :: IO ()
main5 = do
  putStrLn "HC9T5:"
  print $ Circle "Red" 10
  print $ Rectangle "Blue" 5 7


-- HC9T6: Recursive Tweet Data Type
data Tweet = Tweet
  { content :: String
  , likes :: Int
  , comments :: [Tweet]
  } deriving Show

main6 :: IO ()
main6 = do
  putStrLn "HC9T6:"
  let reply = Tweet "Nice post!" 2 []
  let post = Tweet "Check out my new article." 5 [reply]
  print post


-- HC9T7: Engagement Function
engagement :: Tweet -> Int
engagement (Tweet _ l cs) = l + sum (map engagement cs)

main7 :: IO ()
main7 = do
  putStrLn "HC9T7:"
  let t1 = Tweet "Hi" 3 []
  let t2 = Tweet "Nice" 2 [t1]
  print $ engagement t2 -- 5 (2 + 3)


-- HC9T8: Recursive Sequence
data Sequence a = End | Node a (Sequence a) deriving Show

main8 :: IO ()
main8 = do
  putStrLn "HC9T8:"
  let seq1 = Node 1 (Node 2 (Node 3 End))
  print seq1


-- HC9T9: Check for Element in a Sequence
elemSeq :: Eq a => a -> Sequence a -> Bool
elemSeq _ End = False
elemSeq x (Node y rest) = x == y || elemSeq x rest

main9 :: IO ()
main9 = do
  putStrLn "HC9T9:"
  let seq = Node 1 (Node 2 (Node 3 End))
  print $ elemSeq 2 seq  -- True
  print $ elemSeq 5 seq  -- False


-- HC9T10: Binary Search Tree
data BST a = EmptyTree | NodeBST a (BST a) (BST a) deriving Show

main10 :: IO ()
main10 = do
  putStrLn "HC9T10:"
  let tree = NodeBST 10 (NodeBST 5 EmptyTree EmptyTree) (NodeBST 15 EmptyTree EmptyTree)
  print tree


-- Combined main
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
