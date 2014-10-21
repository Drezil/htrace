{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Applicative
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8

import Data.Attoparsec

import Scene.Parser
import Scene.Types

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

validateScene :: [ObjectParser] -> Either String Scene
validateScene obs = do
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

render :: Scene -> IO ()
render s = putStrLn "rendering currently not implemented"

main :: IO ()
main = do
    f <- B.readFile "scenes/test.sce"
    rawScene <- return $ parseScene f
    case rawScene of
        Left error -> putStrLn $ "error Parsing: " ++ error
        Right raw -> do
            scene <- return $ validateScene raw
            case scene of
                Left error -> putStrLn $ "Error: " ++ error
                Right s -> do
                    print s
                    render s
