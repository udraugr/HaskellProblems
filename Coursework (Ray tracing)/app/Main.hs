import Graphics.Gloss.Raster.Field
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss
import Shapes
import Math
import Lights
import Scene


data State = State
        {   curTime           :: Float
            , camPos            :: Vec3
            , camDir            :: Vec3
            -- shape and lights list
            , shapes            :: [Shape]
            , lights            :: [Light] 
        } deriving Show


initState :: Float -> State
initState time = State 
            {   curTime         = time
                , camPos        = Vec3 (0.0) (0.0) (1.0)
                , camDir        = Vec3 (0.0) (0.0) (1.0)
                , shapes        = getShapes time
                , lights        = getLights time 
            }


main :: IO ()
main = do
    let sizeX = 800
    let sizeY = 800
    let bounce = 1
    let min_t = 1.0
    playField
        (InWindow "Ray tracing" (sizeX, sizeY) (0, 0))
        (1, 1)
        1000
        (initState 0)
        (tracePixel sizeX sizeY bounce min_t)
        handleInput
        stepWorldIteration


stepWorldIteration :: Float -> State -> State
stepWorldIteration dt state
 = let  newTime = (curTime state) + dt
        shape = getShapes newTime
        light  = getLights newTime 
   in state
        {
          curTime           = newTime
        , shapes            = shape
        , lights            = light
        }


handleInput :: Event -> State -> State
handleInput event state = state


tracePixel :: Int -> Int -> Int -> Float -> State -> Point -> Color
tracePixel sizeX sizeY bounce min_t state (x, y) = 
    let sX = fromIntegral sizeX
        sY = fromIntegral sizeY
        (Vec3 xD yD zD) = camDir state
        lookDirF = Vec3 (xD + (sX / sY * x)) (yD + (y)) zD
        normLookDir = normalize lookDirF
        lookPos = camPos state
        (Vec3 r g b) = traceRay (lights state)
                                (shapes state)
                                lookPos
                                normLookDir
                                bounce
                                min_t
    in makeColor (r / 255.0) (g / 255.0) (b / 255.0) 1.0


