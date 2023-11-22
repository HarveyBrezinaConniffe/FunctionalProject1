{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Lib
import Shapes
import Codec.Picture
import Render (render,defaultWindow)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Web.Scotty
import Data.IORef
import System.Eval

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
  imgProg <- newIORef "[((translate 0.5 0), rectangle 0.5 0.5, gradient), ((scale 0.25 0.5), circle, gradient)]"
  maskProg <- newIORef "[((scale 1 0.25), circle)]"
  get "/image" $ do
    imgstr <- readIORef imgProg
    maskstr <- readIORef maskProg
    imgCode <- eval imgstr
    raw (render defaultWindow imgCode (MaskDrawing circleMask))
  get "/" $
    html $ renderHtml $
      H.html $
        H.body $ do
          H.h1 "Welcome"
          H.p "Hi"
          H.img H.! A.src "/image"
