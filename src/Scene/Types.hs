module Scene.Types where

import Linear (V3)

type Color = V3 Float
type Intensity = Float

data Camera = Camera
            { eye :: V3 Float
            , center :: V3 Float
            , up :: V3 Float
            , fovy :: Float
            , width :: Int
            , height :: Int
            }

type RecursionDepth = Int

data Background = Background Color

data Ambience = Ambience Color

data Light = Light (V3 Float) Color (Maybe Intensity)

data Material = Material
            { materialAmbience :: V3 Float
            , materialDiffuse :: V3 Float
            , materialSpec :: V3 Float
            , materialShinyness :: Int
            , materialReflection :: Float
            }
            

data Sphere = Sphere
            { sphereCenter :: V3 Float
            , sphereRadius :: Float
            , sphereMaterial :: Material
            }

data Plane = Plane
            { planeCenter :: V3 Float
            , planeNormal :: V3 Float
            , planeMaterial :: Material
            }

data Shading = Flat | Phong

data Mesh = Mesh
            { meshFilename :: String
            , meshShading :: Shading
            , material :: Material
            }

data SceneObject = S Sphere | P Plane | M Mesh | C Camera | L Light | 

