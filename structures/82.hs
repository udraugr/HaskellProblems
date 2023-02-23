getAllCycles :: Int -> Int -> [(Int, Int)] -> [[Int]]
getAllCycles finish pos [] = []
getAllCycles finish pos mas = [ pos : (findCicles !! j) | i <- [0 .. length(mas) - 1],
    let x = fst(mas !! i),
    let y = snd(mas !! i),
    let findCicles = [[y]] ++ getAllCycles finish y (fst(splitAt i mas) ++ snd(splitAt (i + 1) mas)),
    j <- [0 .. length(findCicles) - 1],
    x == pos && ((pos /= finish && y == finish) || not (null $ findCicles !! j) && (last (findCicles !! j) == finish))]


myCycle :: Int -> [(Int, Int)] -> [[Int]]
myCycle n [] = []
myCycle n mas = getAllCycles n n mas


main :: IO ()
main = do
    putStrLn $ show $ myCycle 1 [(1,2),(2,3),(1,3),(3,4),(4,2),(5,6)]
    putStrLn $ show $ myCycle 2 [(1,2),(2,3),(1,3),(3,4),(4,2),(5,6)]
    putStrLn $ show $ myCycle 1 [(1,2),(2,3),(3,6),(3,4),(4,5),(5,3),(6,1)]
    putStrLn $ show $ myCycle 3 [(1,2),(2,3),(3,6),(3,4),(4,5),(5,3),(6,1)]
