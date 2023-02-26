myReturn :: [a] -> Maybe [a]
myReturn [] = Nothing
myReturn l  = Just l


elementAt  :: [a] -> Int -> Maybe a
elementAt mas 1 = Just (mas!!0)
elementAt (_:mas) index = elementAt mas (index - 1)
elementAt [] _ = Nothing


myIter :: [a] -> Int -> Maybe a
myIter mas index = myReturn mas >>= \a -> elementAt a index


myToStr :: Maybe Char -> [Char]
myToStr Nothing = "Nothing"
myToStr (Just c) = [c]


main :: IO ()
main = do
    input <- readFile "input.txt"
    let ans = myToStr $ myIter input 15
    putStrLn ans
    writeFile "output.txt" ans
    putStrLn $ myToStr $ myIter input 1
    putStrLn $ myToStr $ myIter input (-1)
    putStrLn $ myToStr $ myIter input 5
