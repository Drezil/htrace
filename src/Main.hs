{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Applicative
import Control.Arrow
import Codec.Picture.Png
import Codec.Picture.Types
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import Data.Vector hiding ((++),map, foldl, filter, foldl1)
import Data.Word (Word8)
import Data.Maybe
import Linear (V3(..), (^+^), (^*), (^-^), dot, axisAngle, rotate, cross)

import Data.Attoparsec

import Scene.Parser
import Scene.Types

import Debug.Trace

findCamera :: [ObjectParser] -> Either String Camera
findCamera []     = Left "No camera found"
findCamera (a:as) = case a of
                    OpC c -> return c
                    _ -> findCamera as

findDepth :: [ObjectParser] -> Either String RecursionDepth
findDepth []     = Left "No recursion depth defined"
findDepth (a:as) = case a of
                    OpR r -> return r
                    _ -> findDepth as

findAmbience :: [ObjectParser] -> Either String Ambience
findAmbience []     = Left "No ambience light defined"
findAmbience (a:as) = case a of
                    OpA am -> return am
                    _ -> findAmbience as

findBackground :: [ObjectParser] -> Either String Background
findBackground []     = Left "No background color defined"
findBackground (a:as) = case a of
                    OpB b -> return b
                    _ -> findBackground as

filterLights :: [ObjectParser] -> [Light]
filterLights []     = []
filterLights (a:as) = case a of
                      OpL l -> l:filterLights as
                      _ -> filterLights as

filterObjects :: [ObjectParser] -> [Collidable]
filterObjects []     = []
filterObjects (a:as) = case a of
                      OpS s -> S s:filterObjects as
                      OpM m -> M m:filterObjects as
                      OpP p -> P p:filterObjects as
                      _ -> filterObjects as

validateAndParseScene :: B8.ByteString -> Either String Scene
validateAndParseScene f = do
            obs <- parseScene f
            cam <- findCamera obs
            depth <- findDepth obs
            amb <- findAmbience obs
            back <- findBackground obs
            lights <- return $ filterLights obs
            objects <- return $ filterObjects obs
            return $ Scene
                   { ambientLight = amb
                   , sceneCamera = cam
                   , sceneRecursions = depth
                   , sceneBackground = back
                   , sceneLights = lights
                   , sceneObjects = objects
                   }

render :: Int -> Int -> Scene -> Int -> PixelRGB8
render w h s index = case pcolls of
                        [] -> PixelRGB8 (ci br) (ci bg) (ci bb) --no collision -> Background
                        _  -> PixelRGB8 (ci ar) (ci ag) (ci ab) --collission -> git color
        where
            (V3 ar ag ab) = materialAmbience $ getMaterial coll
            (Background (V3 br bg bb)) = sceneBackground s
            pcolls = map fromJust $ filter isJust $ (intersect ray) <$> (sceneObjects s)
            (Collision pos _ coll) = foldl1 min pcolls
            ray = Ray (center cam) $ rotCam x y w h (eye cam) (up cam) (fovy cam)
            cam = sceneCamera s
            y = fromIntegral $ index `mod` w
            x = fromIntegral $ index `div` w
            ci = floor . (*255)

rotCam :: Float -> Float -> Int -> Int -> V3 Float -> V3 Float -> Float -> V3 Float
rotCam x y w h dir up fovy = rotxy 
            where
                rotxy = rotateDegAx (rad $ fovy*dy) (cross up roty) roty
                roty  = rotateDegAx (rad $ fovy*dx) up dir
                dx = (x - (fromIntegral w) / 2)/(fromIntegral w)
                dy = (y - (fromIntegral h) / 2)/(fromIntegral h)
                rad = (*pi).(/180)

rotateDegAx :: Float -> V3 Float -> V3 Float -> V3 Float
rotateDegAx phi axis = rotate q
                where
                    q = axisAngle axis phi

data Ray = Ray (V3 Float) (V3 Float)

data Collision = Collision (V3 Float) Float Collidable
                    deriving (Eq)

instance Ord Collision where
    compare (Collision _ a _) (Collision _ b _) = compare a b

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

main :: IO ()
main = do
    f <- B.readFile "scenes/test.sce"
    case validateAndParseScene f of
      Left error -> putStrLn $ "Error: " ++ error
      Right s -> do
        let (w,h)  = (width . sceneCamera $ s, height . sceneCamera $ s)
            imdata = map (render w h s) [0..w*h-1]
            imvec  = fromList imdata
            im     = generateImage (\x y -> imvec ! (x*w+y)) w h
        print s
        print (w,h)
        writePng "out.png" im
