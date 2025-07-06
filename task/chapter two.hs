boolAnd :: Bool
boolAnd = True && True

boolOr :: Bool
boolOr = False || False

boolNot :: Bool
boolNot = not False

compareFunc :: Bool
compareFunc = 10 < 5

main :: IO ()
main = do
    print boolAnd    -- True
    print boolOr     -- False
    print boolNot    -- True
    print compareFunc -- False
