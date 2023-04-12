module Math(Vec3(..),
            QuadraticRoots(..),
            solveQuadrEquation,
            mulV3,
            dot,
            lenV3,
            normalize)
where


data Vec3 = Vec3 Float Float Float deriving Show
data QuadraticRoots = TwoRoots Float Float | OneRoot Float | NoRoots deriving Show


instance Num Vec3 where
    (+) (Vec3 x1 y1 z1) (Vec3 x2 y2 z2) = Vec3 (x1 + x2) (y1 + y2) (z1 + z2)
    (-) (Vec3 x1 y1 z1) (Vec3 x2 y2 z2) = Vec3 (x1 - x2) (y1 - y2) (z1 - z2)


mulV3 :: Vec3 -> Float -> Vec3
mulV3 (Vec3 x y z) k = Vec3 (x * k) (y * k) (z * k)


dot :: Vec3 -> Vec3 -> Float
dot (Vec3 x1 y1 z1) (Vec3 x2 y2 z2) = x1 * x2 + y1 * y2 + z1 * z2


lenV3 :: Vec3 -> Float
lenV3 vector = sqrt $ dot vector vector


normalize :: Vec3 -> Vec3
normalize vector = mulV3 vector (1.0 / (lenV3 vector))


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
