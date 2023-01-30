pack :: Num a => [Char] -> [(a, Char)]
pack [] = [(0, '\0')]
pack [x] = [(1, x)]
pack (h:str) = 
    let result = pack str
    in case result of
        var | snd (var !! 0) == h -> ( ((fst (result !! 0)) + 1), (h) ) : (tail result)
            | otherwise           -> ( (1), (h) ) : result


main :: IO ()
main = do
    putStrLn $ show $ pack $ "aaaabccaadeeee"