splitAtWords :: (a -> Bool) -> [a] -> [[a]]
splitAtWords _ [] = [[]]
splitAtWords f (x:mas)  | f x == False = (x:(result !! 0)) : [result !! i | i <- [1 .. length result - 1]]
                        | f x == True && length result > 0 && length (result!!0) == 0 = result
                        | otherwise = [] : result
                        where
                            result = splitAtWords f mas

main :: IO ()
main = do
    putStrLn $ show $ splitAtWords (\ x -> elem x [' ', '\t', '\n']) "Hello world"
    putStrLn $ show $ splitAtWords (\ x -> elem x ['o']) "Hello world"
    
