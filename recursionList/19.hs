rotate :: [Char] -> Int -> [Char]
rotate [] _ = []
rotate str 0 = str
rotate str x | x > 0     = rotate (tail(str) ++ head(str):[]) (x - 1)
             | x < 0     = rotate str ((length str) + x)

main :: IO ()
main = do
    putStrLn . show $ rotate  ['a','b','c','d','e','f','g','h'] (-2)
