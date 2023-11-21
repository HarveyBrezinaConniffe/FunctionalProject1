{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Lib
import Shapes
import Codec.Picture
import Render (render,defaultWindow)
import Web.Scotty

--exampleDrawing =  [ ((scale 0.5 0.25)<+>(translate 0.25 0.5)<+>(shear 0.25 0.1)<+>(rotate 0.5), circle) ]
--circleDrawing = [(ident, circle)]
--rectDrawing = [(ident, rectangle 0.1 0.8)]
--ellipseDrawing = [(ident, ellipse 0.5 0.5)]
--polygonDrawing = [(ident, polygon [(polygonEdge (point 0 0) (point 0.5 1)), 
--                                      (polygonEdge (point 0.5 1) (point 1 0)),
--                                      (polygonEdge (point 1 0) (point 0 0))])]
--tPolygonDrawing = [((scale 0.5 0.25)<+>(translate 0.25 0.5)<+>(shear 0.25 0.1)<+>(rotate 0.5), polygon [(polygonEdge (point 0 0) (point 0.5 1)), 
--                                      (polygonEdge (point 0.5 1) (point 1 0)),
--                                      (polygonEdge (point 1 0) (point 0 0))])]

gradient :: Point -> PixelRGB8 
gradient (Vector x y) = PixelRGB8 redVal 255 100 where
  redVal = round ((x/0.5)*255.0)

exampleDrawing = [((translate 0.5 0), rectangle 0.5 0.5, gradient),
               ((scale 0.25 0.5), circle, gradient)]

circleMask = [((scale 1 0.25), circle)]

--main = render "output.png" defaultWindow exampleDrawing (MaskDrawing circleMask)

main = scotty 8080 $ do
  get "/" $ do
    html "Hello World!"
