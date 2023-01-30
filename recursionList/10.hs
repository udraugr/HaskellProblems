encode :: Num a => [Char] -> [(a, Char)]
encode [] = [(0, '\0')]
encode [x] = [(1, x)]
encode (h:str) = 
    let result = encode str
    in case result of
        var | snd (var !! 0) == h -> ( ((fst (result !! 0)) + 1), (h) ) : (tail result)
            | otherwise           -> ( (1), (h) ) : result


main :: IO ()
main = do
    putStrLn $ show $ encode $ "aaaabccaadeeee"