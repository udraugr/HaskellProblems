elementAt  :: [a] -> Int -> Maybe a
elementAt mas 1 = Just (mas!!0)
elementAt (_:mas) index = elementAt mas (index - 1)
elementAt [] index = Nothing

main :: IO ()
main = do
    putStrLn $ show $ elementAt "haskell" 5
    putStrLn $ show $ elementAt [1,2,3] 2
    putStrLn $ show $ elementAt [1,2,3] (-1)
    putStrLn $ show $ elementAt [1,2,3] 8

