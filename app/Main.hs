module Main (main) where

import Lib
import Shapes
import Render (render,defaultWindow)

exampleDrawing =  [ (scale (point 0.5 0.25) <+> translate (point 1.2 0.4), circle) ]
circleDrawing = [(identity, circle)]
rectDrawing = [(identity, rectangle 0.1 0.8)]
ellipseDrawing = [(identity, ellipse 0.5 0.5)]

main = render "output.png" defaultWindow rectDrawing
