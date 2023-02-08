-- lexicographic comparison, not required for this task

compStr :: String -> String -> Bool
compStr [] lhs = True
compStr [] [] = False
compStr rhs [] = False
compStr rhs lhs | (head rhs) < (head lhs) = True 
                | (head rhs) > (head lhs) = False 
                | (head rhs) == (head lhs) = compStr (tail rhs) (tail lhs) 


-- lsort realization

lsortUtils :: Int -> [String] -> [String]
lsortUtils 0 massiv = massiv
lsortUtils n massiv = lsortUtils (n - 1) ([ massiv !! i | i <- [0 .. (length massiv) - 1],
                                                          length (massiv !! i) < length (massiv !! (n - 1))] ++ 
                                            [ massiv !! i | i <- [0 .. (length massiv) - 1],
                                                            length (massiv !! i) >= length (massiv !! (n - 1))])

lsort :: [String] -> [String]
lsort [] = []
lsort massiv = lsortUtils (length massiv) massiv


-- lfsort realization

maxLengthInMassiv :: Int -> [String] -> Int
maxLengthInMassiv len [] = len
maxLengthInMassiv len mas = maxLengthInMassiv (max len (length(head mas))) (tail mas)

lfsortUtils :: [String] -> [String]
lfsortUtils massiv = sortAndCreateByLen [ [ massiv !! i | i <- [0..(length massiv - 1)],
                                                         (length(massiv !! i)) == j] | let k = maxLengthInMassiv 0 massiv,
                                                                                       j <- [1 .. k] ]

sortByLen :: Int -> [[String]] -> [[String]]
sortByLen 0 massiv = massiv
sortByLen n massiv = sortByLen (n - 1) ([ massiv !! i | i <- [0 .. (length massiv) - 1],
                                                        length (massiv !! i) > 0 && length (massiv !! i) > length (massiv !! (n - 1))] ++ 
                                        [ massiv !! i | i <- [0 .. (length massiv) - 1],
                                                        length (massiv !! i) > 0 && length (massiv !! i) <= length (massiv !! (n - 1))])


createByLen :: [[String]] -> [String]
createByLen mas = [ mas !! i !! j | i <- [0 .. (length mas) - 1],
                                    j <- [0 .. (length (mas !! i)) - 1]]

sortAndCreateByLen :: [[String]] -> [String]
sortAndCreateByLen mas = createByLen (reverse (sortByLen (length mas) mas))


lfsort :: [String] -> [String]
lfsort [] = []
lfsort massiv = lfsortUtils (lsort massiv)

--Test in main

main :: IO ()
main = do
    putStrLn . show $ lsort ["abc","de","fgh","de","ijkl","mn","o"]
    putStrLn . show $ lfsort ["abc","de","fgh","de","ijkl","mn","o"]