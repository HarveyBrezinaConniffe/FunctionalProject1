{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad.IO.Class (MonadIO(liftIO))
import Lib
import Shapes
import Codec.Picture
import Render (render,defaultWindow)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Web.Scotty
import Data.IORef
import qualified Data.ByteString.Lazy as B
import Data.Text.Lazy (toStrict)
import Data.Text.Encoding (decodeUtf8)

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

shapesAndColors = [((scale 0.5 0.25)<+>(translate 0.25 0.25), circle, (\(Vector x y) -> PixelRGB8 255 255 255)),
                   ((translate (-0.25) (-0.25))<+>(shear 0.1 0), rectangle 0.3 0.3, (\(Vector x y) -> PixelRGB8 255 100 100)),
                   ((translate 0.25 (-0.25)), ellipse 0.3 0.2, (\(Vector x y) -> PixelRGB8 100 255 100))]

polygons = [((rotate 0.1), polygon [(polygonEdge (point 0 0) (point 0 1)),
                             (polygonEdge (point 0 1) (point 1 0)),
                             (polygonEdge (point 1 0) (point 0 0))], (\(Vector x y) -> PixelRGB8 255 100 100)),
            ((scale 0.3 0.3)<+>(translate (-1) (-0.2)), 
                    polygon [(polygonEdge (point 0 0) (point 0 1)),
                             (polygonEdge (point 0 1) (point 1 2)),
                             (polygonEdge (point 1 2) (point 2 1)),
                             (polygonEdge (point 2 1) (point 2 0)),
                             (polygonEdge (point 2 0) (point 1 (-1))),
                             (polygonEdge (point 1 (-1)) (point 0 0))], (\(Vector x y) -> PixelRGB8 (round ((x/2.0)*255.0)) 100 100)),
            ((scale 0.3 0.3)<+>(shear 0 0.2)<+>(translate (0.5) (-0.6)), 
                    polygon [(polygonEdge (point 0 0) (point 0 1)),
                             (polygonEdge (point 0 1) (point 1 2)),
                             (polygonEdge (point 1 2) (point 2 1)),
                             (polygonEdge (point 2 1) (point 2 0)),
                             (polygonEdge (point 2 0) (point 1 (-1))),
                             (polygonEdge (point 1 (-1)) (point 0 0))], (\(Vector x y) -> PixelRGB8 (round (((y+2.0)/5.0)*255.0)) 100 100))]

gradient :: Point -> PixelRGB8 
gradient (Vector x y) = PixelRGB8 redVal 255 100 where
  redVal = round ((x/0.5)*255.0)

exampleDrawing = [((translate 0.5 0), rectangle 0.5 0.5, gradient),
               ((scale 0.25 0.5), circle, gradient)]

circleMask = [((scale 1 0.5), circle)]

--main = render "output.png" defaultWindow exampleDrawing (MaskDrawing circleMask)

renderImg :: Int -> B.ByteString
renderImg 0 = render defaultWindow exampleDrawing (MaskDrawing circleMask)
renderImg 1 = render defaultWindow exampleDrawing (EmptyMask)
renderImg 2 = render defaultWindow shapesAndColors (EmptyMask)
renderImg 3 = render defaultWindow polygons (EmptyMask)
renderImg 4 = render defaultWindow polygons (MaskDrawing circleMask)

imgCodeString :: String -> String
imgCodeString "0" = "[((translate 0.5 0), rectangle 0.5 0.5, gradient), ((scale 0.25 0.5), circle, gradient)]"
imgCodeString "2" = "[((scale 0.5 0.25)<+>(translate 0.25 0.25), circle, (\\x y -> PixelRGB8 255 255 255)), ((translate -0.25 -0.25)<+>(shear 0.1 0)), rectangle 0.3 0.3, (\\(Vector x y) -> PixelRGB8 255 100 100), ((translate 0.25 -0.25), ellipse 0.3 0.2, (\\(Vector x y) -> PixelRGB8 100 255 100))]"
imgCodeString "3" = "[((rotate 0.1), polygon [(polygonEdge (point 0 0) (point 0 1)), (polygonEdge (point 0 1) (point 1 0)), (polygonEdge (point 1 0) (point 0 0))], (\\(Vector x y) -> PixelRGB8 255 100 100)), ((scale 0.3 0.3)<+>(translate (-1) (-0.2)), polygon [(polygonEdge (point 0 0) (point 0 1)), (polygonEdge (point 0 1) (point 1 2)), (polygonEdge (point 1 2) (point 2 1)), (polygonEdge (point 2 1) (point 2 0)), (polygonEdge (point 2 0) (point 1 (-1))), (polygonEdge (point 1 (-1)) (point 0 0))], (\\(Vector x y) -> PixelRGB8 (round ((x/2.0)*255.0)) 100 100)), ((scale 0.3 0.3)<+>(shear 0 0.2)<+>(translate (0.5) (-0.6)), polygon [(polygonEdge (point 0 0) (point 0 1)), (polygonEdge (point 0 1) (point 1 2)), (polygonEdge (point 1 2) (point 2 1)), (polygonEdge (point 2 1) (point 2 0)), (polygonEdge (point 2 0) (point 1 (-1))), (polygonEdge (point 1 (-1)) (point 0 0))], (\\(Vector x y) -> PixelRGB8 (round (((y+2.0)/5.0)*255.0)) 100 100))]"
imgCodeString "4" = "[((rotate 0.1), polygon [(polygonEdge (point 0 0) (point 0 1)), (polygonEdge (point 0 1) (point 1 0)), (polygonEdge (point 1 0) (point 0 0))], (\\(Vector x y) -> PixelRGB8 255 100 100)), ((scale 0.3 0.3)<+>(translate (-1) (-0.2)), polygon [(polygonEdge (point 0 0) (point 0 1)), (polygonEdge (point 0 1) (point 1 2)), (polygonEdge (point 1 2) (point 2 1)), (polygonEdge (point 2 1) (point 2 0)), (polygonEdge (point 2 0) (point 1 (-1))), (polygonEdge (point 1 (-1)) (point 0 0))], (\\(Vector x y) -> PixelRGB8 (round ((x/2.0)*255.0)) 100 100)), ((scale 0.3 0.3)<+>(shear 0 0.2)<+>(translate (0.5) (-0.6)), polygon [(polygonEdge (point 0 0) (point 0 1)), (polygonEdge (point 0 1) (point 1 2)), (polygonEdge (point 1 2) (point 2 1)), (polygonEdge (point 2 1) (point 2 0)), (polygonEdge (point 2 0) (point 1 (-1))), (polygonEdge (point 1 (-1)) (point 0 0))], (\\(Vector x y) -> PixelRGB8 (round (((y+2.0)/5.0)*255.0)) 100 100))]"

maskCodeString :: String -> String
maskCodeString "0" = "[((scale 1 0.25), circle)]"
maskCodeString "2" = "EmptyMask"
maskCodeString "3" = "EmptyMask"
maskCodeString "4" = "[((scale 1 0.5), circle)]"

descriptionString :: String -> String
descriptionString "2" = "An drawing showing 3 shapes (circle, ellipse and rectangle) along with 3 transforms (translate, scale and shear). Using solid colors."
descriptionString "3" = "A drawing showing various polygons with translate, scale and shear transformations applied. Using color functions to draw gradients."
descriptionString "4" = "A drawing showing various polygons with translate, scale and shear transformations applied. Using color functions to draw gradients. Masked with a circle."

main = do
  progNum <- newIORef 1
  scotty 8080 $ do
    get "/:imgNum" $ do
      imgNum <- param "imgNum"
      let imgURL = "/image/"++imgNum
      let descString = descriptionString imgNum
      let imgString = imgCodeString imgNum
      let maskString = maskCodeString imgNum
      html $ renderHtml $
        H.html $
          H.body $ do
            H.h1 "Welcome"
            H.h2 "Image Description"
            H.p (H.string descString)
            H.h2 "Main Image Code"
            H.p (H.string imgString)
            H.h2 "Mask Image Code"
            H.p (H.string maskString)
            H.img H.! A.src (H.stringValue imgURL)
            H.form H.! A.action "/2" $ do
              H.input H.! A.type_ "submit" H.! A.value "Shapes and Colors"
            H.form H.! A.action "/3" $ do
              H.input H.! A.type_ "submit" H.! A.value "Polygons"
            H.form H.! A.action "/4" $ do
              H.input H.! A.type_ "submit" H.! A.value "Masked Polygons"
    get "/image/:imgNum" $ do
      imgNum <- param "imgNum"
      raw $ renderImg imgNum
