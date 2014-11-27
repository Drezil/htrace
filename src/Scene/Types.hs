module Scene.Types where

import Linear (V3)
import Data.IntMap
import qualified Data.Vector as V
import Data.ByteString

type Color = V3 Float
type Intensity = Float

data Camera = Camera
            { eye       :: V3 Float
            , center    :: V3 Float
            , up        :: V3 Float
            , fovy      :: Float
            , width     :: Int
            , height    :: Int
            , lowerLeft :: V3 Float
            , xDir      :: V3 Float
            , yDir      :: V3 Float
            }
                deriving (Show, Eq)

type RecursionDepth = Int

data Background = Background
                { bgColor :: Color
                }
                deriving (Show, Eq)

data Ambience = Ambience
                { ambColor :: Color
                }
                deriving (Show, Eq)

data Light = Light (V3 Float) Color (Maybe Intensity)
                deriving (Show, Eq)

data Material = Material
            { materialAmbience   :: V3 Float
            , materialDiffuse    :: V3 Float
            , materialSpec       :: V3 Float
            , materialShinyness  :: Float
            , materialReflection :: Float
            }
                deriving (Show, Eq)


data Sphere = Sphere
            { sphereCenter   :: V3 Float
            , sphereRadius   :: Float
            , sphereMaterial :: Material
            }
                deriving (Show, Eq)

data Plane = Plane
            { planeCenter   :: V3 Float
            , planeNormal   :: V3 Float
            , planeMaterial :: Material
            }
                deriving (Show, Eq)

data Shading = Flat | Phong
                deriving (Show, Eq)

data UIMesh = UIMesh
            { uimeshFilename :: ByteString
            , uimeshShading  :: Shading
            , uimaterial     :: Material
            }
                deriving (Show, Eq)

data Mesh = Mesh
            { meshShading     :: Shading
            , meshMaterial    :: Material
            , meshVertices    :: IntMap (V3 Float)
            , meshFaces       :: IntMap (V3 Int)
            , meshNormals     :: IntMap (V3 Float)
            , meshFaceNormals :: IntMap (V3 Float)
            , meshBounds      :: BoundingBox
            }
            deriving (Show, Eq)

data BoundingBox = BoundingBox
            { boundX         :: (Float, Float)
            , boundY         :: (Float, Float)
            , boundZ         :: (Float, Float)
            }
            deriving (Show, Eq)

data ObjectParser = OpS Sphere
                  | OpP Plane
                  | OpM UIMesh
                  | OpI Mesh
                  | OpC Camera
                  | OpL Light
                  | OpR RecursionDepth
                  | OpA Ambience
                  | OpB Background
                  deriving (Show, Eq)

data Collidable = S Sphere
                | P Plane
                | M Mesh
                deriving (Show, Eq)

data Scene = Scene
           { ambientLight    :: Ambience
           , sceneCamera     :: Camera
           , sceneLights     :: [Light]
           , sceneBackground :: Background
           , sceneRecursions :: RecursionDepth
           , sceneObjects    :: [Collidable]
           }
                deriving (Show, Eq)

getMaterial :: Collidable -> Material
getMaterial (S (Sphere _ _ m)) = m
getMaterial (P (Plane _ _ m)) = m
getMaterial (M (Mesh _ m _ _ _ _ _)) = m
