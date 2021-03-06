{-# LANGUAGE OverloadedStrings, BangPatterns #-}
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
import Control.Lens.Operators
import Data.IntMap as IM
import Prelude as P
import qualified Data.List as L

import Scene.Parser
import Scene.Types

import Debug.Trace as D

data Ray = Ray (V3 Float) (V3 Float)

-- | Collision position normal distance object
data Collision = Collision (V3 Float) (V3 Float) Float Collidable
                    deriving (Eq)

instance Ord Collision where
    compare (Collision _ _ a _) (Collision _ _ b _) = compare a b

epsilon :: Float
epsilon = 0.00001

render :: Int -> Int -> Scene -> Int -> V3 Word8
render w h s index = V3 (ci cr) (ci cg) (ci cb)
        where
            (V3 cr cg cb) = getColorFromRay (sceneRecursions s) ray s

            ray@(Ray co _) = camRay x y (sceneCamera s)
            y = fromIntegral $ h - (index `div` w) - 1
            x = fromIntegral $ index `mod` w
            ci = floor . (clamp 0 255) . (*255)
            --wrong format:
            --Ray (eye cam) $ rotCam x y w h (center cam - eye cam) (up cam) (fovy cam)
            --cam = sceneCamera s

getColorFromRay :: Int -> Ray -> Scene -> V3 Float
getColorFromRay refLeft ray@(Ray raypos raydir) s = clamp 0 1 <$> color
  where color =
          case raytrace ray s of
            Nothing -> bgColor $ sceneBackground s
            Just c@(Collision cpos cnor _ obj) ->
                -- ambient lighting
                ((ambColor . ambientLight $ s) * (materialAmbience . getMaterial $ obj))
                -- + diffuse/spec lighting
                + (foldl1 (+) $ (diffuseAndSpec c s raypos) <$> sceneLights s)
                -- + reflect
                + reflection
                where
                    ! reflection = if refLeft == 0 || (materialReflection . getMaterial) obj == 0 then
                                     V3 0 0 0
                                 else
                                     reflcolor ^* (materialReflection . getMaterial $ obj)
                                     where
                                        reflcolor = getColorFromRay (refLeft-1) (Ray (cpos + (cnor ^* (2 * epsilon))) refldir) s
                                        refldir = normalize ((eye3 - 2 *!! outer cnor cnor) !* raydir)

-- | Collision-Information, Scene, view-position, light
diffuseAndSpec :: Collision -> Scene -> V3 Float-> Light -> V3 Float
diffuseAndSpec (Collision pos n _ obj) s co (Light lpos color int) =
            case blocked of
                Nothing                     -> diff + spec
                Just (Collision _ _ dist _) -> if dist < norm lightdir
                                            then
                                                V3 0 0 0 --light is blocked -> no lighting from here.
                                            else
                                                diff + spec
        where
            spec = if dot n ld < 0 || dot r v < 0
                    then V3 0 0 0
                    else i * (dot r v ** materialShinyness mat) *^ color * materialSpec mat
            r = (dot n ld * 2 *^ n) - ld
            ! ld = normalize lightdir
            v = normalize $ co - pos
            diff = if dot n ld < 0
                    then V3 0 0 0
                    else i * dot n ld *^ color * materialDiffuse mat
            mat = getMaterial obj
            ! blocked = raytrace (Ray pos lightdir) s
            ! lightdir = (lpos - pos)
            i = case int of
                    Nothing -> 1
                    Just a -> 1 --a TODO: What is light-intensity and how does it relate to lighting?

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
                    possibleCollisions = P.map fromJust $ P.filter isJust $ (intersect r) <$> sceneObjects s

camRay :: Float -> Float -> Camera -> Ray
camRay x y c = Ray (eye c) (normalize $ lowerLeft c + x *^ xDir c + y *^ yDir c - eye c)

rotateDegAx :: Float -> V3 Float -> V3 Float -> V3 Float
rotateDegAx phi axis = rotate q
                where
                    q = axisAngle axis phi

intersect :: Ray -> Collidable -> Maybe Collision
intersect (Ray ro rd) s@(S (Sphere sc sr _)) = if (d > 0 && int > 0) then
                                                Just $ Collision pos (normalize $ pos - sc) int s
                                              else
                                                Nothing
                where
                    a = dot rd rd
                    b = 2 * dot rd oc
                    c = dot oc oc - sr*sr
                    ! d = b * b - 4 * a * c
                    oc = ro - sc
                    pos = ro + (rd ^* int)
                    int = case ints of
                        [] -> 0
                        a  -> P.foldl1 min a
                    ints = P.filter (uncurry (&&).(&&&) (>epsilon) (not.isNaN)) [(-b-(sqrt d))/(2*a),(-b+(sqrt d))/(2*a)]
intersect (Ray ro rd) p@(P (Plane pc pn _)) = if det == 0 || t < epsilon
                                                          then Nothing
                                                          else Just $ Collision pos pn t p
                where
                    pos = ro + t *^ rd'
                    ! det = dot rd' pn
                    t = (dot (pc - ro) pn)/det
                    rd' = normalize rd
intersect r@(Ray ro rd) m@(M (Mesh s _ v f vn fn b)) = case catMaybes . elems $ possHits of
                                [] -> Nothing
                                a  -> Just . P.head . L.sort $ a
                where
                    possHits = if inBounds b r then case s of
                        Flat -> hitsFlat v fn `IM.mapWithKey` f
                        Phong -> hitsPhong v vn fn `IM.mapWithKey` f
                        else
                            IM.fromList []
                    inBounds :: BoundingBox -> Ray -> Bool
                    inBounds bound r = a < b
                        where
                           (a,b) = L.foldl' (\(a,b) (c,d) -> (min a c, max b d)) (0,infty)
                                   [ intersectBounds (boundX bound) r (V3 1 0 0)
                                   , intersectBounds (boundY bound) r (V3 0 1 0)
                                   , intersectBounds (boundZ bound) r (V3 0 0 1)]
                    intersectBounds :: (Float, Float) -> Ray -> V3 Float -> (Float,Float)
                    intersectBounds (min, max) (Ray ro rd) n = 
                        if det == 0 || tmin < epsilon
                            then if tmax < epsilon
                                 then (0,infty)
                                 else (tmin, tmax)
                            else (tmin, tmax)
                        where
                            ! det = dot rd' n
                            rd' = normalize rd
                            tmin = (dot ((min *^ n) - ro) n)/det
                            tmax = (dot ((max *^ n) - ro) n)/det

                    infty = 999999999999999999
                    hitsFlat :: IntMap (V3 Float) -> IntMap (V3 Float) -> Int -> V3 Int -> Maybe Collision
                    hitsFlat verts norm f (V3 w1 w2 w3) =
                        if det == 0 || t < epsilon || not det2
                            then Nothing
                            else Just $ Collision (pos+10*epsilon *^ (norm IM.! f)) (norm IM.! f) t m
                        where
                            ! det = dot rd' (norm IM.! f) --do we hit the plane
                            rd' = normalize rd
                            t = (dot ((verts IM.! w1) - ro) (norm IM.! f))/det --when do we hit the plane
                            pos = ro + t *^ rd'                                --where do we hit the plane
                            v1 = (verts IM.! w2) - (verts IM.! w1)
                            v2 = (verts IM.! w3) - (verts IM.! w1)
                            det2m = fromJust $ inv33 $ transpose $ V3 v1 v2 (norm IM.! f)  -- base-change-matrix into triangle-coordinates
                            det2v = det2m !* (pos - (verts IM.! w1))
                            -- fromJust is justified as we only make a base-change and all 3
                            -- vectors are linear independent.
                            det2 =     det2v ^. _x >= 0 && det2v ^. _y >= 0
                                    && det2v ^. _x + det2v ^. _y <= 1
                    hitsPhong :: IntMap (V3 Float) -> IntMap (V3 Float) -> IntMap (V3 Float) -> Int -> V3 Int -> Maybe Collision
                    hitsPhong verts vnorm fnorm f (V3 w1 w2 w3) =
                        if det == 0 || t < epsilon || not det2
                            then Nothing
                            else Just $ Collision (pos + 10*epsilon *^ vns) vns t m
                        where
                            ! det = dot rd' (fnorm IM.! f)
                            rd' = normalize rd
                            t = (dot ((verts IM.! w1) - ro) (fnorm IM.! f))/det
                            pos = ro + t *^ rd'
                            v1 = (verts IM.! w2) - (verts IM.! w1)
                            v2 = (verts IM.! w3) - (verts IM.! w1)
                            det2m = fromJust $ inv33 $ transpose $ V3 v1 v2 (fnorm IM.! f)
                            det2v = det2m !* (pos - (verts IM.! w1))
                            det2 =    det2v ^. _x >= 0 && det2v ^. _y >= 0
                                   && det2v ^. _x + det2v ^. _y <= 1
                            vns = normalize $
                                  --(0.5 - sqrt ((sqr (det2v ^. _x) + sqr (det2v ^. _y))/2)) *^ (vnorm IM.! w1) -- << this one should be correct
                                  (1 - det2v ^. _x - det2v ^. _y) *^ (vnorm IM.! w1)                            -- << this one works better..
                                + ((det2v ^. _x)/2) *^ (vnorm IM.! w2)
                                + ((det2v ^. _y)/2) *^ (vnorm IM.! w3)
                            sqr = \x -> x * x


-- deprecated - wrong calculation of rays.
rotCam :: Float -> Float -> Int -> Int -> V3 Float -> V3 Float -> Float -> V3 Float
rotCam x y w h dir up fovy = rotxy
            where
                rotxy = rotateDegAx (rad $ fovy*dy) (cross up roty) roty
                roty  = rotateDegAx (rad $ fovy*dx*(-1)) up dir
                dx = (x - (fromIntegral w) / 2)/(fromIntegral w)
                dy = (y - (fromIntegral h) / 2)/(fromIntegral h)
                rad = (*pi).(/180)
