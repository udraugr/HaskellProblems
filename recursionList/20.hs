removeAt  ::  Int -> [Char] ->(Char, [Char])
removeAt  _ []  = error "no input"
removeAt x str  | x > 0 && x <= length str = (str !! (x - 1), deleteChar str (x - 1))
                | otherwise            = error "Invalid index"
                where
                    deleteChar (c:str) 0 = str
                    deleteChar [] _ = []
                    deleteChar (c:str) x = c:deleteChar str (x - 1) 


main :: IO ()
main = do
    putStrLn . show $ removeAt (2) ['a','b','c','d','e','f','g','h'] 
