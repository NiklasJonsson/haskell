module Shapes
( Point(..)
, Shape(..)
, surface
, distanceToCenter
, centerPoint
, nudge
) where

data Point = Point Float Float deriving (Show)
data Shape = Circle Point Float | Rectangle Point Point deriving (Show)

surface :: Shape -> Float
surface (Circle _ r) = pi * r^2
surface (Rectangle (Point x1 y1) (Point x2 y2)) = (abs $ x2 - x1) * (abs $ y2 - y1)

distanceToCenter :: Shape -> Shape -> Float
distanceToCenter shape1 shape2 = dist (centerPoint shape1) (centerPoint shape2)
	where dist (x1, y1) (x2,y2) = sqrt $ (x2 - x1)^2 + (y2 - y1)^2

centerPoint :: Shape -> (Float, Float)
centerPoint (Circle (Point x1 x2) _) = (x1, x2)
centerPoint (Rectangle (Point x1 y1) (Point x2 y2)) = ((y2 - y1) / 2 , (x2 - x1) / 2)

nudge :: Shape -> Float -> Float -> Shape
nudge (Circle (Point x y) r) a b = Circle (Point (x+a) (y+b)) r
nudge (Rectangle (Point x1 y1) (Point x2 y2)) a b = Rectangle (Point (x1+a) (y1+b)) (Point (x2+a) (y2+b))
