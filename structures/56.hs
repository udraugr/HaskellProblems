data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Show, Eq)

checkSym :: Tree(Char) -> Tree(Char) -> Bool
checkSym Empty Empty = True
checkSym Empty (Branch _ lhs rhs) = False
checkSym (Branch _ lhs rhs) Empty = False
checkSym (Branch _ lhs1 rhs1) (Branch _ lhs2 rhs2) = checkSym lhs1 rhs2 && checkSym rhs1 lhs2

symmetric :: Tree(Char) -> Bool
symmetric Empty = True
symmetric (Branch _ lhs rhs) = checkSym lhs rhs

main :: IO ()
main = do
    putStrLn $ show $ symmetric (Branch 'x' (Branch 'x' Empty Empty) Empty)
    putStrLn $ show $ symmetric (Branch 'x' (Branch 'x' Empty Empty) (Branch 'x' Empty Empty))
