combinations :: Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations n massiv = [ massiv !! i : x | i <- [0..(length massiv - 1)],
                                            x <- combinations (n - 1) (drop (i + 1) massiv) ]

main :: IO ()
main = do
    putStrLn . show $ combinations 3 "abcdef"
