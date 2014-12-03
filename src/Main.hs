{-# LANGUAGE OverloadedStrings, BangPatterns #-}
module Main where

import Codec.Picture.Png
import Codec.Picture.Types
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import Data.Vector.Storable hiding ((++),map, foldl, filter, foldl1)
import Linear (V3(..))
import Data.Word (Word8)
import Data.Functor
import Data.Maybe
import Data.Traversable
import Control.Parallel.Strategies
import Control.Monad.Trans
import Control.Monad.Trans.Either
import Control.DeepSeq
import System.Environment
import System.FilePath.Posix

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
                      OpI m -> M m:filterObjects as
                      OpP p -> P p:filterObjects as
                      _ -> filterObjects as

validateAndParseScene :: B8.ByteString -> FilePath -> EitherT String IO Scene
validateAndParseScene f p = do
            obs <- hoistEither $ parseScene f
            obs' <- initializeMeshes p obs
            cam <- hoistEither $ findCamera obs'
            depth <- hoistEither $ findDepth obs'
            amb <- hoistEither $ findAmbience obs'
            back <- hoistEither $ findBackground obs'
            lights <- return $ filterLights obs'
            objects <- return $ filterObjects obs'
            return $ Scene
                   { ambientLight = amb
                   , sceneCamera = cam
                   , sceneRecursions = depth
                   , sceneBackground = back
                   , sceneLights = lights
                   , sceneObjects = objects
                   }

initializeMeshes :: FilePath -> [ObjectParser] -> EitherT String IO [ObjectParser]
initializeMeshes p = traverse (initializeMeshes' p)
            where
                initializeMeshes' :: FilePath -> ObjectParser -> EitherT String IO ObjectParser
                initializeMeshes' p (OpM (UIMesh f s m)) = 
                                            let filename = p </> (B8.unpack f) in
                                            do
                                            d <- lift $ B.readFile filename
                                            lift $ print filename
                                            mesh <- hoistEither $ parseMesh s m d
                                            return mesh
                initializeMeshes' _ a = return a
                    

instance NFData PixelRGB8
    where
        rnf (PixelRGB8 r g b) = r `seq` g `seq` b `seq` ()

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> putStrLn "please specify a scene to render"
        (a:_) -> do
            putStrLn $ "reading and parsing "++ show a
            !f <- B.readFile a
            r <- runEitherT $ validateAndParseScene f (dropFileName a)
            print r
            case r of
              Left error -> putStrLn $ "Error: " ++ error
              Right s -> do
                putStrLn "redering..."
                let (w,h)  = (width . sceneCamera $ s, height . sceneCamera $ s)
                    imvec = fromList ((render w h s <$> [0..w*h-1]) `using` parListChunk w rseq)
                    im     = generateImage (v3ToPixel w imvec) w h
                writePng "out.png" im

v3ToPixel :: Int -> Vector (V3 Word8) -> Int -> Int -> PixelRGB8
v3ToPixel w vec x y = PixelRGB8 r g b
            where
                V3 r g b = vec ! (y*w+x)
