module Scene(getShapes,
             getLights)
where


import Shapes
import Lights
import Math


getShapes :: Float -> [Shape]
getShapes time = [ 
                   Sphere
                    (Vec3 (0.0) (-1.0) (3.0))
                    0.4
                    (Vec3 255.0 0.0 0.0)
                    500.0 0.2
                    ,
                  --  Sphere
                  --   (Vec3 (2.0) (0.0) (4.0))
                  --   1.0
                  --   (Vec3 0.0 0.0 255.0)
                  --   500.0 0.3
                  --   ,
                  --  Sphere
                  --   (Vec3 (-2.0) (0.0) (4.0))
                  --   1.0
                  --   (Vec3 0.0 255.0 0.0)
                  --   10.0 0.4
                  --   ,
                  --  Sphere
                  --   (Vec3 (0.0) (-5001.0) (0.0))
                  --   5000.0
                  --   (Vec3 255.0 255.0 0.0)
                  --   1000.0 0.5
                  --   ,
                   Plane
                    (Vec3 0.0 0.0 (20.0))
                    (normalize (Vec3 0.0 0.0 (1.0)))
                    (Vec3 255.0 0.0 255.0)
                    100.0 0.4
                    ,
                   Cone
                    (Vec3 (-1.0) (-1.0) (12.0))
                    0.5
                    (normalize (Vec3 1.0 0.0 0.0))
                    (Vec3 0.0 255.0 255.0)
                    100.0 0.4
                    ,
                   Cylinder
                    (Vec3 4.0 0.0 (4.0))
                    (normalize (Vec3 0.0 1.0 1.0))
                    1.0
                    (Vec3 255.0 255.0 255.0)
                    100.0 0.4
                 ]


getLights :: Float -> [Light]
getLights time = [ 
                   Light (Vec3 (2.0) (1.0) (0.0)) (0.6)
                  --  ,
                  --  Light (Vec3 (-30.0) (-30.0) (15)) (0.3)
                 ]