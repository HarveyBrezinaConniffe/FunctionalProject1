module Shapes(
  Shape, Point, Vector(..), Transform, Drawing,
  point, getX, getY,
  empty, circle, square, rectangle, ellipse, polygon, polygonEdge,
  ident, translate, rotate, scale, shear, (<+>),
  getColor, getMask, Mask(..))  where

import Data.Matrix
import Codec.Picture
 
-- Utilities

data Vector = Vector Double Double
              deriving Show
vector = Vector

cross :: Vector -> Vector -> Double
cross (Vector a b) (Vector a' b') = a * a' + b * b'

getX (Vector x y) = x
getY (Vector x y) = y

-- Shapes

type Point  = Vector

point :: Double -> Double -> Point
point = vector

pointToHomogenousVec :: Point -> (Matrix Double)
pointToHomogenousVec (Vector x y) = fromList 3 1 [x, y, 1]

homogenousVecToPoint :: (Matrix Double) -> Point
homogenousVecToPoint mat = Vector (getElem 1 1 mat) (getElem 2 1 mat)

data HorizontalRay = HorizontalRay Double Double
                     deriving Show
horizontalRay = HorizontalRay

data PolygonEdge = PolygonEdge Point Point
                   deriving Show
polygonEdge = PolygonEdge

data Shape = Empty
           | Circle
           | Square
           | Rectangle Double Double
           | Ellipse Double Double
           | Polygon [PolygonEdge]
             deriving Show

empty, circle, square :: Shape

empty = Empty
circle = Circle
square = Square
rectangle = Rectangle
ellipse = Ellipse
polygon = Polygon

-- Transformations

data Transform = Transform (Matrix Double)
  deriving Show

ident         = Transform (fromList 3 3 [1, 0, 0, 0, 1, 0, 0, 0, 1])
translate x y = Transform (fromList 3 3 [1, 0, -x, 0, 1, -y, 0, 0, 1])
scale     x y = Transform (fromList 3 3 [1/x, 0, 0, 0, 1/y, 0, 0, 0, 1])
shear     x y = Transform (fromList 3 3 [1/k, (-x)/k, 0, (-y)/k, 1/k, 0, 0, 0, 1]) where
  k = 1 * 1 - x * y 
rotate angle  = Transform (fromList 3 3 [(cos angle)/k, (-(-sin angle))/k, 0, (-(sin angle))/k, (cos angle)/k, 0, 0, 0, 1]) where
  k = (cos angle) * (cos angle) - (-sin angle) * (sin angle)

(<+>) :: Transform -> Transform -> Transform
(Transform m1) <+> (Transform m2) = Transform (multStd m1 m2)

transform :: Transform -> Point -> Point
transform (Transform mat) p = homogenousVecToPoint (multStd mat (pointToHomogenousVec p))

-- Drawings

type Drawing = [(Transform, Shape, (Point -> PixelRGB8))]
data Mask = EmptyMask
            | MaskDrawing [(Transform, Shape)]

-- interpretation function for drawings

getColor :: Point -> [(Transform, Shape, (Point -> PixelRGB8))] -> PixelRGB8
getColor _ [] = PixelRGB8 0 0 0
getColor p ((t, shape, colorFunc):xs) = case insideShape (transform t p) shape of
  True -> colorFunc (transform t p)
  False -> getColor p xs

getMask :: Point -> Mask -> Bool
getMask _ EmptyMask = True
getMask _ (MaskDrawing []) = False
getMask p (MaskDrawing ((t, shape):xs)) = case insideShape (transform t p) shape of
  True -> True
  False -> getMask p (MaskDrawing xs)

-- inside :: Point -> Drawing -> Bool
-- inside p d = any (inside1 p) d

-- inside1 :: Point -> (Transform, Shape, (Point -> Int -> Int -> Int)) -> Bool
-- inside1 p (t, s, _) = insides (transform t p) s

insideShape :: Point -> Shape -> Bool
p `insideShape` Empty = False
p `insideShape` Circle = distance p <= 1
p `insideShape` Square = maxnorm  p <= 1
(Vector x y) `insideShape` (Rectangle width height) = (x >= 0 && x <= width) && (y >= 0 && y <= height)
(Vector x y) `insideShape` (Ellipse width height) = (((x**2)/((width/2)**2))+((y**2)/((height/2)**2))) <= 1
p `insideShape` (Polygon edges) = pointInPolygon p edges

pointInPolygon :: Point -> [PolygonEdge] -> Bool
pointInPolygon (Vector x y) edges = odd (numIntersects (Vector x y) edges)

numIntersects :: Point -> [PolygonEdge] -> Int
numIntersects (Vector x y) edges = sum (map fromEnum intersectList) where
  intersectList = map (rayIntersectsLineSegment (HorizontalRay x y)) edges 

rayIntersectsLineSegment :: HorizontalRay -> PolygonEdge -> Bool
rayIntersectsLineSegment (HorizontalRay start_x start_y) (PolygonEdge (Vector x1 y1) (Vector x2 y2)) =
  lineCrossesXAxis point1 point2 && 0 <= xIntersect point1 point2 where
    point1 = (point (x1-start_x) (y1-start_y))
    point2 = (point (x2-start_x) (y2-start_y))

lineCrossesXAxis :: Point -> Point -> Bool
lineCrossesXAxis (Vector _ y1) (Vector _ y2) = y1*y2 <= 0

xIntersect :: Point -> Point -> Double
xIntersect (Vector x1 y1) (Vector x2 y2) = (-c)/m where
  m = (y2-y1)/((x2-x1)+0.0001)
  c = y2-(m*x2)

distance :: Point -> Double
distance (Vector x y) = sqrt ( x**2 + y**2 )

maxnorm :: Point -> Double
maxnorm (Vector x y) = max (abs x) (abs y)
