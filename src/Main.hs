{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Applicative
import Codec.Picture.Png
import Codec.Picture.Types
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import Data.Vector hiding ((++),map)
import Data.Word (Word8)

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
render w h s index = trace (show (x,y)) PixelRGB8 255 (fromIntegral x) (fromIntegral y)
        where
            y = index `mod` w
            x = index `div` w

data Ray = Ray (V3 Float) (V3 Float)

data Collision = Collision (V3 Float) Float Collidable

intersect :: Ray -> Collidable -> Maybe Collision
intersect (Ray ro rd) (S (Sphere sc sr _) = undefined
intersect _ = undefined

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
