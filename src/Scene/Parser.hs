module Scene.Parser where

import Data.Attoparsec
import Data.Functor
import Data.ByteString as B
import Data.ByteString.Char8 as B8
import Scene.Types

partitionEithers :: [Either a b] -> ([a],[b])
partitionEithers e = go e ([],[])
    where
        go (Left a:as)  (xs,ys) = go as (a:xs,ys)
        go (Right b:bs) (xs,ys) = go bs (xs,b:ys)

parseScene :: FilePath -> IO ([String],[SceneObject])
parseScene f = do
            s <- B.readFile f
            return . partitionEithers $ eitherResult . parse parseObject <$> B8.lines s

parseObject :: Parser SceneObject
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
            _        -> undefined

parseCamera :: Parser SceneObject
parseCamera = do
        pos <- parseVector

parseVector :: Parser (V3 Float)
parseVector = do
        undefined
