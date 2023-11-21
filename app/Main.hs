module Main (main) where

import Lib
import Shapes
import Render (render,defaultWindow)

exampleDrawing =  [ ((scale 0.5 0.25)<+>(translate 0.25 0.5)<+>(shear 0.25 0.1)<+>(rotate 0.5), circle) ]
circleDrawing = [(ident, circle)]
rectDrawing = [(ident, rectangle 0.1 0.8)]
ellipseDrawing = [(ident, ellipse 0.5 0.5)]
polygonDrawing = [(ident, polygon [(polygonEdge (point 0 0) (point 0.5 1)), 
                                      (polygonEdge (point 0.5 1) (point 1 0)),
                                      (polygonEdge (point 1 0) (point 0 0))])]
tPolygonDrawing = [((scale 0.5 0.25)<+>(translate 0.25 0.5)<+>(shear 0.25 0.1)<+>(rotate 0.5), polygon [(polygonEdge (point 0 0) (point 0.5 1)), 
                                      (polygonEdge (point 0.5 1) (point 1 0)),
                                      (polygonEdge (point 1 0) (point 0 0))])]

main = render "output.png" defaultWindow tPolygonDrawing
