{-# LANGUAGE OverloadedStrings #-}
module Scene.Parser (parseScene, parseMesh) where

import Control.Applicative
import Data.Attoparsec.ByteString.Char8 as A8
import Data.Functor
import Data.IntMap as IM
import Data.IntSet
import qualified Data.Foldable as F
import Data.ByteString as B
import Data.ByteString.Char8 as B8
import Linear
import Prelude as P

import Scene.Types

import Debug.Trace as D

partitionEithers :: [Either a b] -> ([a],[b])
partitionEithers e = go e ([],[])
    where
        go (Left a:as)  (xs,ys) = go as (a:xs,ys)
        go (Right b:bs) (xs,ys) = go bs (xs,b:ys)
        go []           (xs,ys) = (xs,ys)

preprocess :: ByteString -> ByteString
preprocess = B8.unlines . clean . B8.lines
            where
              clean :: [ByteString] -> [ByteString]
              clean (a:as) = if B8.all isWhitespace a
                                || B8.head a == '#'
                             then clean as else a:clean as
              clean [] = []
              isWhitespace :: Char -> Bool
              isWhitespace ' '  = True
              isWhitespace '\t' = True
              isWhitespace '\n' = True
              isWhitespace '\r' = True
              isWhitespace _    = False


parseScene :: ByteString -> Either String [ObjectParser]
parseScene s = parseOnly (many parseObject) (preprocess s)

parseObject :: Parser ObjectParser
parseObject = do
        t    <- string "camera" <|>
                string "depth" <|>
                string "background" <|>
                string "ambience" <|>
                string "light" <|>
                string "sphere" <|>
                string "plane" <|>
                string "mesh"
        case t of
            "camera" -> parseCamera
            "depth" -> do
                        skipSpace
                        d <- decimal
                        endOfLine
                        return $ OpR d
            "background" -> do
                        c <- parseVector
                        endOfLine
                        return $ OpB (Background c)
            "ambience" -> do
                        c <- parseVector
                        endOfLine
                        return $ OpA (Ambience c)
            "light" -> do
                        p <- parseVector
                        skipSpace
                        c <- parseVector
                        intensity <- double <|> return 0
                        i <- return $ if intensity == 0
                                         then Nothing
                                         else Just (fromRational . toRational $ intensity)
                        endOfLine
                        return $ OpL (Light p c i)
            "sphere" -> parseSphere
            "plane"  -> parsePlane
            "mesh"   -> parseRawMesh
            _ -> undefined

parseCamera :: Parser ObjectParser
parseCamera = do
        eye'      <- parseVector
        center'   <- parseVector
        up'       <- parseVector
        skipSpace
        fovy'     <- double
        skipSpace
        width'    <- decimal
        skipSpace
        height'   <- decimal
        endOfLine
        let     xDir' = (normalize $ cross (center' ^-^ eye') up') ^* (im_width / w)
                yDir' = (normalize $ cross xDir' view) ^* (im_height / h)
                lowerLeft' = center' ^-^ (0.5 * w *^ xDir')
                                     ^-^ (0.5 * h *^ yDir')
                im_height = 2*dist* tan (0.5*(frtr fovy')/180*pi)
                im_width = w/h * im_height
                view = (center' ^-^ eye')
                dist = norm view
                w = fromIntegral width'
                h = fromIntegral height'
                frtr = fromRational . toRational
        return $ OpC $ Camera
                        { eye = eye'
                        , center = center'
                        , up = up'
                        , fovy = (fromRational . toRational) fovy'
                        , width = width'
                        , height = height'
                        , lowerLeft = lowerLeft'
                        , xDir = xDir'
                        , yDir = yDir'
                        }

parsePlane :: Parser ObjectParser
parsePlane = do
        c <- parseVector
        n <- parseVector
        m <- parseMaterial
        endOfLine
        return $ OpP Plane
               { planeCenter = c
               , planeNormal = normalize n
               , planeMaterial = m
               }

parseSphere :: Parser ObjectParser
parseSphere = do
        p <- parseVector
        skipSpace
        r <- double
        m <- parseMaterial
        endOfLine
        return $ OpS Sphere
               { sphereCenter = p
               , sphereRadius = (fromRational . toRational) r
               , sphereMaterial = m
               }

parseMaterial :: Parser Material
parseMaterial = do
        a <- parseVector
        d <- parseVector
        s <- parseVector
        skipSpace
        sh <- double
        skipSpace
        r <- double
        return $ Material
               { materialAmbience = a
               , materialDiffuse = d
               , materialSpec = s
               , materialShinyness = (fromRational . toRational) sh
               , materialReflection = (fromRational . toRational) r
               }

parseVector :: Parser (V3 Float)
parseVector = do
        skipSpace
        a <- double
        skipSpace
        b <- double
        skipSpace
        c <- double
        return $ V3 (f a) (f b) (f c)
            where
              f = fromRational . toRational --convert Double to Float

parseRawMesh :: Parser ObjectParser
parseRawMesh = do
        skipSpace
        name <- takeTill isSpace
        skipSpace
        shading <- string "FLAT" <|> string "PHONG"
        skipSpace
        mat <- parseMaterial
        let shading' = case shading of
                        "FLAT"  -> Flat
                        "PHONG" -> Phong
        return $ OpM UIMesh
                      { uimeshFilename = name
                      , uimeshShading  = shading'
                      , uimaterial     = mat
                      }

parseMesh :: Shading -> Material -> ByteString -> Either String ObjectParser
parseMesh s m f = parseOnly (parseMesh' s m) (preprocess f)

parseMesh' :: Shading -> Material -> Parser ObjectParser
parseMesh' s m = do
            string "OFF"
            skipSpace
            v <- decimal
            skipSpace
            f <- decimal
            skipSpace
            _ <- decimal --ignored in our OFF-Files
            skipSpace
            verts <- D.trace (show v ++ " verts") $ A8.count v parseVector
            faces <- D.trace (show f ++ " faces") $ A8.count f parseTriangle
            -- whatever should be parsed afterwards in OFF..
            let
                mv = IM.fromList $ P.zip [0..] verts
                mf = IM.fromList $ P.zip [0..] faces
                mfn = normal mv <$> mf
                normal :: IntMap (V3 Float) -> V3 Int -> V3 Float
                normal verts (V3 v1 v2 v3) = normalize $ cross (verts ! v2 - verts ! v1)
                                                               (verts ! v3 - verts ! v1)
                                                                -- maybe * (-1)
                mn = IM.fromList $ P.zip [0..] $ vnormal mfn mf <$> [0..v]
                vnormal :: IntMap (V3 Float) -> IntMap (V3 Int) -> Int -> V3 Float
                vnormal norms faces i = normalize $ F.foldl' (+) (V3 0 0 0) $ (!) norms <$> fs
                                        --TODO: weight sum with opening-angle!
                            where
                                fs = keys $ IM.filter (\(V3 a b c) -> P.any (==i) [a,b,c]) faces
                bounds = f b
                       where
                          f ((V3 a b c),(V3 x y z)) = BoundingBox
                                                      { boundX = (a,x)
                                                      , boundY = (b,y)
                                                      , boundZ = (c,z)
                                                      }
                          b = F.foldl' minmax (V3 (-infty) (-infty) (-infty), V3 infty infty infty) mv
                          minmax (maxin,minin) vec = (max <$> maxin <*> vec, min <$> minin <*> vec)
                          infty = 9999999999999 :: Float
            return $ OpI Mesh
                        { meshShading     = s
                        , meshMaterial    = m
                        , meshVertices    = mv
                        , meshFaces       = mf
                        , meshNormals     = mn
                        , meshFaceNormals = mfn
                        , meshBounds      = bounds
                        }

parseTriangle :: Parser (V3 Int)
parseTriangle = do
        skipSpace
        _ <- string "3"
        skipSpace
        a <- decimal
        skipSpace
        b <- decimal
        skipSpace
        c <- decimal
        return $ V3 a b c

