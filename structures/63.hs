data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Show, Eq)

completeBinaryTree :: Int -> Tree(Char)
completeBinaryTree 0 = Empty
completeBinaryTree x    | x > 1 = (Branch 'x' (l_tree x) (r_tree x))
                        | x == 1 = Branch 'x' Empty Empty
                            where
                                x' = round(((fromIntegral(x) - 1.0 - fromIntegral(div (x - 1) 2)) / 2.0) * 2.0)
                                l_tree xn    | x' == 0 = Empty
                                             | x' > 0 = (completeBinaryTree (x'))
                                r_tree xn    | xn - 1 - x' == 0 = Empty
                                             | xn - 1 - x' > 0 = (completeBinaryTree (xn - 1 - x'))  

quantityNodes :: Tree(Char) -> Int
quantityNodes Empty = 0
quantityNodes (Branch _ lhs rhs) = 1 + quantityNodes rhs + quantityNodes lhs

checkCompleteBinaryTree :: Int -> Tree(Char) -> Bool
checkCompleteBinaryTree 1 (Branch _ Empty Empty) = True
checkCompleteBinaryTree n (Branch _ lhs rhs) | (l_tree n) && (r_tree n) = True
                                             | otherwise = False
                                             where
                                                n' = round(((fromIntegral(n) - 1.0 - fromIntegral(div (n - 1) 2)) / 2.0) * 2.0)
                                                l_tree nn     | n' == 0 && lhs == Empty = True
                                                              | n' > 0 = checkCompleteBinaryTree n' lhs
                                                              | otherwise = False
                                                r_tree nn     | nn - 1 - n' == 0 && rhs == Empty = True
                                                              | nn - 1 - n' > 0 = checkCompleteBinaryTree (nn - 1 - n') rhs
                                                              | otherwise = False
                                                

isCompleteBinaryTree :: Tree(Char) -> Bool                                                                                                                      
isCompleteBinaryTree Empty = True                                                                                                                           
isCompleteBinaryTree (Branch x lhs rhs) = checkCompleteBinaryTree
                                            (quantityNodes $ (Branch x lhs rhs))
                                            (Branch x lhs rhs)

main :: IO ()
main = do
    putStrLn $ show $ completeBinaryTree 1
    putStrLn $ show $ completeBinaryTree 2
    putStrLn $ show $ completeBinaryTree 3
    putStrLn $ show $ completeBinaryTree 4
    putStrLn $ show $ completeBinaryTree 7
    putStrLn $ show $ completeBinaryTree 10
    putStrLn $ show $ isCompleteBinaryTree $ Empty
    putStrLn $ show $ isCompleteBinaryTree $ Branch 'x' Empty Empty
    putStrLn $ show $ isCompleteBinaryTree $ Branch 'x' (Branch 'x' Empty Empty) Empty
    putStrLn $ show $ isCompleteBinaryTree $ Branch 'x' (Branch 'x' Empty (Branch 'x' Empty Empty)) Empty
    putStrLn $ show $ isCompleteBinaryTree $ Branch 'x' (Branch 'x' Empty Empty) (Branch 'x' Empty Empty)
