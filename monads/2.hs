myButLast :: [a] -> Maybe a
myButLast [a, _] = Just a
myButLast (_:mas) = myButLast mas
myButLast [] = Nothing

main :: IO ()
main = do
    putStrLn $ show $ myButLast ""
    putStrLn $ show $ myButLast ['a'..'z']
    putStrLn $ show $ myButLast [1,2,3,4]

