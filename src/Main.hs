{-# LANGUAGE OverloadedStrings, BangPatterns #-}
module Main where

import Codec.Picture.Png
import Codec.Picture.Types
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import Data.Vector hiding ((++),map, foldl, filter, foldl1)
import Data.Functor
import Data.Maybe
import Control.Parallel.Strategies
import Control.DeepSeq
import System.Environment

import Scene.Parser
import Scene.Renderer
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

instance NFData PixelRGB8
    where
        rnf (PixelRGB8 r g b) = r `seq` g `seq` b `seq` ()

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> putStrLn "please specify a scene to render"
        (a:_) -> do
            !f <- B.readFile a
            case validateAndParseScene f of
              Left error -> putStrLn $ "Error: " ++ error
              Right s -> do
                let (w,h)  = (width . sceneCamera $ s, height . sceneCamera $ s)
                    !imdata = (render w h s <$> [0..w*h-1]) `using` parListChunk w rdeepseq
                    imvec  = fromList imdata
                    im     = generateImage (\x y -> imvec ! (x*w+(h-y-1))) w h
                writePng "out.png" im
