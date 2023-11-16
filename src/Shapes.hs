module Shapes(
  Shape, Point, Vector, Transform, Drawing,
  point, getX, getY,
  empty, circle, square, rectangle, ellipse, polygon, polygonEdge,
  identity, translate, rotate, scale, (<+>),
  inside)  where

-- Utilities

data Vector = Vector Double Double
              deriving Show
vector = Vector

cross :: Vector -> Vector -> Double
cross (Vector a b) (Vector a' b') = a * a' + b * b'

mult :: Matrix -> Vector -> Vector
mult (Matrix r0 r1) v = Vector (cross r0 v) (cross r1 v)

invert :: Matrix -> Matrix
invert (Matrix (Vector a b) (Vector c d)) = matrix (d / k) (-b / k) (-c / k) (a / k)
  where k = a * d - b * c

-- 2x2 square matrices are all we need.
data Matrix = Matrix Vector Vector
              deriving Show

matrix :: Double -> Double -> Double -> Double -> Matrix
matrix a b c d = Matrix (Vector a b) (Vector c d)

getX (Vector x y) = x
getY (Vector x y) = y

-- Shapes

type Point  = Vector

point :: Double -> Double -> Point
point = vector

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

data Transform = Identity
           | Translate Vector
           | Scale Vector
           | Compose Transform Transform
           | Rotate Matrix
             deriving Show

identity = Identity
translate = Translate
scale = Scale
rotate angle = Rotate $ matrix (cos angle) (-sin angle) (sin angle) (cos angle)
t0 <+> t1 = Compose t0 t1

transform :: Transform -> Point -> Point
transform Identity                   x = x
transform (Translate (Vector tx ty)) (Vector px py)  = Vector (px - tx) (py - ty)
transform (Scale (Vector tx ty))     (Vector px py)  = Vector (px / tx)  (py / ty)
transform (Rotate m)                 p = invert m `mult` p
transform (Compose t1 t2)            p = transform t2 $ transform t1 p

-- Drawings

type Drawing = [(Transform,Shape)]

-- interpretation function for drawings

inside :: Point -> Drawing -> Bool
inside p d = any (inside1 p) d

inside1 :: Point -> (Transform, Shape) -> Bool
inside1 p (t,s) = insides (transform t p) s

insides :: Point -> Shape -> Bool
p `insides` Empty = False
p `insides` Circle = distance p <= 1
p `insides` Square = maxnorm  p <= 1
(Vector x y) `insides` (Rectangle width height) = (x >= 0 && x <= width) && (y >= 0 && y <= height)
(Vector x y) `insides` (Ellipse width height) = (((x**2)/((width/2)**2))+((y**2)/((height/2)**2))) <= 1
p `insides` (Polygon edges) = pointInPolygon p edges

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

testShape = (scale (point 10 10), circle)
