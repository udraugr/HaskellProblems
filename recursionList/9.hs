pack :: [Char] -> [String]
pack [] = [[]]
pack [x] = [[x]]
pack str = 
    let result = pack . tail $ str
    in case result of
        var | var !! 0 !! 0 == (head str) -> ((head str : result !! 0) : (tail result))
            | otherwise                   -> (head str : "") : result


main :: IO ()
main = do
    putStrLn $ show $ pack $ "aaaabccaadeeee"
    
