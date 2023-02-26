compress = foldr (\ x ans -> if ((length ans) > 0 && (ans !! 0) /= x) || (length ans) == 0 then x:ans else ans) []

main :: IO ()
main = do
    putStrLn $ show$ compress "aaaabccaadeeee"
    putStrLn $ show$ compress "1112223334445556667778889990"
    putStrLn $ show$ compress "s"
    putStrLn $ show$ compress ""

