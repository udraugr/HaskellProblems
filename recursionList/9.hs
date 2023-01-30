pack :: [Char] -> [String]
pack [] = [[]]
pack [x] = [[x]]
pack (h:str) = 
    let result = pack str
    in case result of
        var | var !! 0 !! 0 == h -> ((h : result !! 0) : (tail result))
            | otherwise                   -> (h : "") : result


main :: IO ()
main = do
    putStrLn $ show $ pack $ "aaaabccaadeeee"
    
