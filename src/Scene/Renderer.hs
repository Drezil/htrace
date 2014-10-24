{-# LANGUAGE OverloadedStrings #-}
module Scene.Renderer (render) where

import Control.Applicative
import Control.Arrow
import Codec.Picture.Png
import Codec.Picture.Types
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import Data.Vector hiding ((++),map, foldl, filter, foldl1)
import Data.Word (Word8)
import Data.Maybe
import Linear

import Scene.Parser
import Scene.Types

import Debug.Trace

data Ray = Ray (V3 Float) (V3 Float)

data Collision = Collision (V3 Float) Float Collidable
                    deriving (Eq)

instance Ord Collision where
    compare (Collision _ a _) (Collision _ b _) = compare a b

render :: Int -> Int -> Scene -> Int -> PixelRGB8
render w h s index = case pcolls of
                        [] -> PixelRGB8 (ci br) (ci bg) (ci bb) --no collision -> Background
                        _  -> PixelRGB8 (ci ar) (ci ag) (ci ab) --collission -> git color
        where
            (V3 ar ag ab) = materialAmbience $ getMaterial coll
            (Background (V3 br bg bb)) = sceneBackground s
            pcolls = map fromJust $ filter isJust $ (intersect ray) <$> (sceneObjects s)
            (Collision pos _ coll) = foldl1 min pcolls
            ray = camRay x y (sceneCamera s) --Ray (eye cam) $ rotCam x y w h (center cam ^-^ eye cam) (up cam) (fovy cam)
            cam = sceneCamera s
            y = fromIntegral $ index `mod` w
            x = fromIntegral $ index `div` w
            ci = floor . (*255)

camRay :: Float -> Float -> Camera -> Ray
camRay x y c = Ray (eye c) (lowerLeft c ^+^ x *^ xDir c ^+^ y *^ yDir c ^-^ eye c)

rotateDegAx :: Float -> V3 Float -> V3 Float -> V3 Float
rotateDegAx phi axis = rotate q
                where
                    q = axisAngle axis phi

intersect :: Ray -> Collidable -> Maybe Collision
intersect (Ray ro rd) s@(S (Sphere sc sr _)) = if (d > 0 && int > 0) then
                                                Just (Collision (ro ^+^ (rd ^* int)) int s)
                                              else
                                                Nothing
                                where
                                    a = dot rd rd
                                    b = 2 * dot rd oc
                                    c = dot oc oc - sr*sr
                                    d = b * b - 4 * a * c
                                    oc = ro ^-^ sc
                                    int = case ints of
                                            [] -> 0
                                            a  -> foldl1 min a
                                    ints = filter (uncurry (&&).(&&&) (>0.00001) (not.isNaN)) [(-b-(sqrt d))/(2*a),(-b+(sqrt d))/(2*a)]
intersect _ _ = undefined


-- deprecated - wrong calculation of rays.
rotCam :: Float -> Float -> Int -> Int -> V3 Float -> V3 Float -> Float -> V3 Float
rotCam x y w h dir up fovy = rotxy
            where
                rotxy = rotateDegAx (rad $ fovy*dy) (cross up roty) roty
                roty  = rotateDegAx (rad $ fovy*dx*(-1)) up dir
                dx = (x - (fromIntegral w) / 2)/(fromIntegral w)
                dy = (y - (fromIntegral h) / 2)/(fromIntegral h)
                rad = (*pi).(/180)

