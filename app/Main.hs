module Main (main) where

import Lib
import Shapes
import Render (render,defaultWindow)

exampleDrawing =  [ (scale (point 0.5 0.25) <+> translate (point 1.2 0.4), circle) ]
circleDrawing = [(identity, circle)]
rectDrawing = [(identity, rectangle 0.1 0.8)]
ellipseDrawing = [(identity, ellipse 0.5 0.5)]
polygonDrawing = [(identity, polygon [(polygonEdge (point 0 0) (point 0.5 1)), 
                                      (polygonEdge (point 0.5 1) (point 1 0)),
                                      (polygonEdge (point 1 0) (point 0 0))])]

main = render "output.png" defaultWindow polygonDrawing
