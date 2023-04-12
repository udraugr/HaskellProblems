module Shapes(Shape(..),
              traceRay,
              getNearest,
              rayCast)
where


import Math
import Lights



data Shape = 
        Sphere
        {   center            :: Vec3, -- центр сферы
            radius            :: Float, -- радиус сферы
            color             :: Vec3, -- цвет
            specular          :: Float, -- коэффициент зеркальности
            reflective        :: Float -- коэффициент отражения
        }
        | Plane
        {   pos               :: Vec3, -- точка на плоскости
            normal            :: Vec3, -- нормаль поверхности
            color             :: Vec3, -- цвет
            specular          :: Float, -- коэффициент зеркальности
            reflective        :: Float -- коэффициент отражения
        }
        | Cone
        {   center            :: Vec3, -- центр конуса
            tangens           :: Float, -- тангенс конуса
            axis              :: Vec3, -- нормализорованная ось конуса
            color             :: Vec3, -- цвет
            specular          :: Float, -- коэффициент зеркальности
            reflective        :: Float -- коэффициент отражения
        }
        | Cylinder
        {   center            :: Vec3, -- центр цилиндра
            axis              :: Vec3, -- нормализорованная ось цилиндра
            radius            :: Float, -- радиус цилиндра
            color             :: Vec3, -- цвет
            specular          :: Float, -- коэффициент зеркальности
            reflective        :: Float -- коэффициент отражения
        }
        deriving Show


data RayCastInfo = RayCastInfo {
    t                         :: Float,
    normalR                   :: Vec3,
    colorR                    :: Vec3,
    specularR                 :: Float,
    reflectiveR               :: Float
} deriving Show


getInvalidRayCastInfo :: Float -> RayCastInfo
getInvalidRayCastInfo _ = RayCastInfo (-1.0) (Vec3 0.0 0.0 0.0) (Vec3 0.0 0.0 0.0) 0.0 0.0


getNearest :: [RayCastInfo] -> Float -> RayCastInfo
getNearest [] _     = getInvalidRayCastInfo (-1.0)
getNearest [ray] min_t
    | (t ray) >= min_t = ray
    | otherwise = getInvalidRayCastInfo (-1.0)
getNearest (x:xs) min_t
    | (t nearestFromList) >= min_t && (t x) >= min_t && (t nearestFromList) > (t x) = x
    | (t nearestFromList) >= min_t && (t x) >= min_t && (t nearestFromList) <= (t x) = nearestFromList
    | (t nearestFromList) >= min_t && (t x) < min_t = nearestFromList
    | (t nearestFromList) < min_t && (t x) >= min_t = x
    | otherwise = getInvalidRayCastInfo (-1.0)
    where
        nearestFromList = getNearest xs min_t


getNewDir :: Vec3 -> Vec3 -> Vec3
getNewDir normal lookDir = normalize ((mulV3 normal (2.0 * (dot normal lookDir))) - lookDir)


-- трассировка лучей
traceRay :: [Light] -> [Shape] -> Vec3 -> Vec3 -> Int -> Float -> Vec3
traceRay lights shapes lookPos lookDir bounce min_t
    | (t nearestShape) < 0.0 = (Vec3 0.0 0.0 0.0) -- если луч не попал ни в один объект, возвращаем черный цвет
    | bounce <= 0 || (reflectiveR nearestShape) <= 0.0 = (calcColor nearestShape lights shapes lookPos lookDir) -- если это последнее отражение просто возвращем цвет
    | otherwise = mulV3 (calcColor nearestShape lights shapes lookPos lookDir) (1.0 - (reflectiveR nearestShape)) -- если количество отражений еще не равно глубине отражений
                    + mulV3 (traceRay lights -- мы трасируем отраженный от поверхности луч
                        shapes -- и складываем два цвет первого отражения умноженный на 1 - коэффициент отражения
                        (lookPos + mulV3 lookDir (t nearestShape)) -- и возвращенный цвет отражения умноженный на коэффициент отражения
                        (getNewDir (normalR nearestShape) (mulV3 lookDir (-1.0)))
                        (bounce - 1) (0.05)) (reflectiveR nearestShape)
    where
        nearestShape = (getNearest ([compliteInfo (rayCast i lookPos lookDir) i lookPos lookDir min_t | i <- shapes]) min_t)


compliteInfo :: QuadraticRoots -> Shape -> Vec3 -> Vec3 -> Float -> RayCastInfo
compliteInfo NoRoots _ _ _ _ = getInvalidRayCastInfo (-1.0)
compliteInfo (OneRoot t) shape lookPos lookDir min_t
    | t > min_t  = RayCastInfo  t
                                (getNormal shape t lookDir lookPos)
                                (color shape)
                                (specular shape)
                                (reflective shape)
    |otherwise = getInvalidRayCastInfo (-1.0)
compliteInfo (TwoRoots t1 t2) shape lookPos lookDir min_t   
    |t1 > min_t && t2 > min_t = RayCastInfo (min t1 t2) 
                                            (getNormal shape 
                                                        (min t1 t2) 
                                                        lookDir
                                                        lookPos)
                                            (color shape)
                                            (specular shape)
                                            (reflective shape)
    |t1 > min_t || t2 > min_t = RayCastInfo (max t1 t2)
                                            (getNormal shape
                                                        (max t1 t2)
                                                        lookDir
                                                        lookPos)
                                            (color shape)
                                            (specular shape)
                                            (reflective shape)
    |otherwise = getInvalidRayCastInfo (-1.0)


-- Вычисляем пересечения с фигурами
-- Сфера
-- lookPos - это точка точка обзора
-- lookDir - это вектор направленный через пиксель
rayCast :: Shape -> Vec3 -> Vec3 -> QuadraticRoots
rayCast (Sphere center radius _ _ _) lookPos lookDir = 
    solveQuadrEquation a b c --решаем квадратное уравнение с полученными коэффициентами
    where
        a = dot lookDir lookDir
        b = 2.0 * (dot (lookPos - center) lookDir)
        c = (dot (lookPos - center) (lookPos - center)) - radius * radius


-- Плоскость
-- Для плоскости без решения квадратного уравнения
rayCast (Plane pos normal _ _ _) lookPos lookDir  | abs (dot normal lookDir) < 0.00001 = OneRoot (-1.0)
                                                  | otherwise = OneRoot ((-1.0 * (dot (lookPos - pos) normal)) / (dot lookDir normal))


-- Конус
rayCast (Cone center tangens axis _ _ _) lookPos lookDir = 
    solveQuadrEquation a b c -- решаем квадратное уравнение с полученными коэффициентами
    where
        a = (dot lookDir lookDir) - ((dot lookDir axis) ^ 2) * (tangens ^ 2 + 1.0)
        b = 2.0 * ((dot lookDir (lookPos - center)) - (tangens ^ 2 + 1.0) * (dot (lookPos - center) axis) * (dot lookDir axis))
        c = (dot (lookPos - center) (lookPos - center)) - (tangens ^ 2 + 1.0) * (dot (lookPos - center) axis) ^ 2


-- Цилиндр
rayCast (Cylinder center axis radius _ _ _) lookPos lookDir =
    solveQuadrEquation a b c -- решаем квадратное уравнение с полученными коэффициентами
    where
        a = (dot lookDir lookDir) - ((dot lookDir axis) ^ 2)
        b = 2.0 * ((dot lookDir (lookPos - center)) - (dot (lookPos - center) axis) * (dot lookDir axis))
        c = (dot (lookPos - center) (lookPos - center)) - ((dot (lookPos - center) axis) ^ 2) - radius ^ 2


-- Get normales


getNormal :: Shape -> Float -> Vec3 -> Vec3 -> Vec3
getNormal (Plane _ normal _ _ _) _ lookDir _
	| dot lookDir normal < 0.0 = normal
	| otherwise = mulV3 normal (-1.0)


getNormal (Sphere center _ _ _ _) t lookDir lookPos = normalize ((lookPos + mulV3 lookDir t) - center)


getNormal (Cone center tangens axis _ _ _) t lookDir lookPos = 
    normalize (p - center + mulV3 axis (-m * (1.0 + tangens ^ 2))) 
    where
        p = lookPos + mulV3 lookDir t
        m = (dot lookDir axis) * t + dot (lookPos - center) axis


getNormal (Cylinder center axis radius _ _ _) t lookDir lookPos = 
    normalize (p - center + mulV3 axis (-m))
    where
        p = lookPos + mulV3 lookDir t
        m = (dot lookDir axis) * t + dot (lookPos - center) axis


-- Расчет цвета с учетом света и тени
calcColor :: RayCastInfo -> [Light] -> [Shape] -> Vec3 -> Vec3 -> Vec3
calcColor (RayCastInfo t normal color specular reflective) lights shapes lookPos lookDir = 
    mulV3 color (min intensive 1.0)
    where
        intensive = 0.2 + sum [ getIntensive (RayCastInfo t normal color specular reflective) i shapes lookPos lookDir | i <- lights]


-- получение интенсивности для конкретной точки
getIntensive :: RayCastInfo -> Light -> [Shape] -> Vec3 -> Vec3 -> Float
getIntensive (RayCastInfo t normal _ specular _) (Light position intensity) shapes lookPos lookDir
    | (isInShadow (Light position intensity) shapes t lookPos lookDir) == True = 0.0
    | otherwise = (diffusionIntensive intensity normal l) + (specularIntesive intensity normal l (mulV3 lookDir (-1.0)) specular)
    where
        l =  position - (lookPos + mulV3 lookDir t)

-- диффузное освещение
diffusionIntensive :: Float -> Vec3 -> Vec3 -> Float
diffusionIntensive intensity normal l   | (dot normal l) < 0.0 = 0.0
                                        | otherwise = intensity * (dot normal l) / ((lenV3 normal) * (lenV3 l))


--зеркальное освещение
specularIntesive :: Float -> Vec3 -> Vec3 -> Vec3 -> Float -> Float
specularIntesive intensity normal l lookDir specular    | specular < 0.0 || dot r lookDir <= 0.0 = 0.0
                                                        | otherwise = intensity * ( (dot r lookDir) / ( (lenV3 r) * (lenV3 lookDir) ) ) ** specular
                                                        where
                                                            r = mulV3 normal (2.0 * (dot normal l)) - l


-- проверка находится ли точка в тени
isInShadow :: Light -> [Shape] -> Float -> Vec3 -> Vec3 -> Bool
isInShadow (Light position intensity) shapes t lookPos lookDir =
    checkResultShadows [rayCast i p newDir | i <- shapes] l
    where
        p = (lookPos + mulV3 lookDir t)
        newDir = position - p 
        l = lenV3 newDir



checkResultShadows :: [QuadraticRoots] -> Float -> Bool
checkResultShadows (root:roots) l | checkOneResult root l == True   = True
                                  | otherwise                       = checkResultShadows roots l
checkResultShadows [] _                                             = False



checkOneResult :: QuadraticRoots -> Float -> Bool
checkOneResult NoRoots l                                        = False
checkOneResult (OneRoot t) l        | t > 0.01 && t < l         = True
                                    | otherwise                 = False
checkOneResult (TwoRoots t1 t2) l   | (t1 > 0.01 && t1 < l) 
                                      || (t2 > 0.01 && t2 < l)    = True
                                    | otherwise                 = False