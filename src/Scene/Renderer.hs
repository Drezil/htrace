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

-- | Collision position normal distance object
data Collision = Collision (V3 Float) (V3 Float) Float Collidable
                    deriving (Eq)

instance Ord Collision where
    compare (Collision _ _ a _) (Collision _ _ b _) = compare a b

render :: Int -> Int -> Scene -> Int -> PixelRGB8
render w h s index = PixelRGB8 (ci cr) (ci cg) (ci cb)
        where
            (V3 cr cg cb) =
                case raytrace ray s of
                  Nothing -> bgColor $ sceneBackground s
                  Just c@(Collision pos _ _ obj) ->
                         -- ambient lighting
                         ((*) <$> (ambColor . ambientLight $ s) <*> (materialAmbience . getMaterial $ obj))
                         -- + diffuse lighting
                         ^+^ (foldl1 (^+^) $ (diffuse c s) <$> sceneLights s)
                         -- + reflections - TODO
            ray = camRay x y (sceneCamera s)
            y = fromIntegral $ index `mod` w
            x = fromIntegral $ index `div` w
            ci = floor . (clamp 0 255) . (*255)
            --wrong format:
            --Ray (eye cam) $ rotCam x y w h (center cam ^-^ eye cam) (up cam) (fovy cam)
            --cam = sceneCamera s

diffuse :: Collision -> Scene -> Light -> V3 Float
diffuse (Collision pos n _ obj) s (Light lpos color int) =
            case blocked of
                Nothing                   -> ill
                Just (Collision _ _ dist _) -> if dist < norm lightdir
                                            then
                                                V3 0 0 0 --light is blocked -> no lighting from here.
                                            else
                                                ill
        where
            ill = (*) (dot n $ normalize lightdir) <$> ((*) <$> color ^* i <*> materialDiffuse mat)
            mat = getMaterial obj
            blocked = raytrace (Ray pos lightdir) s
            lightdir = (lpos ^-^ pos)
            i = case int of
                    Nothing -> 1
                    Just a -> a

clamp :: Ord a => a -> a -> a -> a
clamp min max x
        | x < min = min
        | x > max = max
        | otherwise = x

raytrace :: Ray -> Scene -> Maybe Collision
raytrace r s = case possibleCollisions of
                    [] -> Nothing
                    _  -> Just $ foldl1 min possibleCollisions
               where
                    possibleCollisions :: [Collision]
                    possibleCollisions = map fromJust $ filter isJust $ (intersect r) <$> (sceneObjects s)

camRay :: Float -> Float -> Camera -> Ray
camRay x y c = Ray (eye c) (lowerLeft c ^+^ x *^ xDir c ^+^ y *^ yDir c ^-^ eye c)

rotateDegAx :: Float -> V3 Float -> V3 Float -> V3 Float
rotateDegAx phi axis = rotate q
                where
                    q = axisAngle axis phi

intersect :: Ray -> Collidable -> Maybe Collision
intersect (Ray ro rd) s@(S (Sphere sc sr _)) = if (d > 0 && int > 0) then
                                                Just (Collision pos (normalize $ pos ^-^ sc) int s)
                                              else
                                                Nothing
                                where
                                    a = dot rd rd
                                    b = 2 * dot rd oc
                                    c = dot oc oc - sr*sr
                                    d = b * b - 4 * a * c
                                    oc = ro ^-^ sc
                                    pos = ro ^+^ (rd ^* int)
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
