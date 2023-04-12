module Lights(Light(..))
where


import Math


data Light = Light {
    position        :: Vec3,
    intensity       :: Float
} deriving Show