myReverse = foldl (\ ans x -> x:ans) []

main :: IO ()
main = do
    putStrLn $ show$ myReverse  "abc"
    putStrLn $ show$ myReverse  "1234567890"
    putStrLn $ show$ myReverse  [1, 2, 3, 4, 5, 6]
    putStrLn $ show$ myReverse  "s"
