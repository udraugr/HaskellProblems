data QuadraticRoots = TwoRoots Float Float | OneRoot Float | NoRoots deriving Show

solveQuadrEquation :: Float -> Float -> Float -> QuadraticRoots
solveQuadrEquation a b c    | a /= 0.0 = solveSecondDegree a b c
                            | a == 0.0 && b /= 0.0 = solveFirstDegree b c
                            | a == 0.0 && b == 0.0 = NoRoots
                                where
                                    solveSecondDegree a b c =
                                        let disc = b * b - 4.0 * a * c
                                        in case disc of
                                            var | var > 0.0 -> (TwoRoots (((-b) + (sqrt disc)) / (2.0 * a)) ((-b - (sqrt disc)) / (2.0 * a)) )
                                                | var == 0.0 -> (OneRoot (((-b) + (sqrt disc)) / (2.0 * a)) )
                                                | otherwise -> NoRoots
                                    solveFirstDegree b c = (OneRoot ((-c) / b) )
            

main :: IO ()
main = putStrLn $ show $ solveQuadrEquation (-1.0) (-2.0) (6.0)
