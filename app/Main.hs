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

gradient :: Point -> PixelRGB8 
gradient (Vector x y) = PixelRGB8 redVal 255 100 where
  redVal = round ((x/0.5)*255.0)

exampleDrawing = [((translate 0.5 0), rectangle 0.5 0.5, gradient),
               ((scale 0.25 0.5), circle, gradient)]

circleMask = [((scale 1 0.25), circle)]

--main = render "output.png" defaultWindow exampleDrawing (MaskDrawing circleMask)

renderImg :: Int -> B.ByteString
renderImg 0 = render defaultWindow exampleDrawing (MaskDrawing circleMask)
renderImg 1 = render defaultWindow exampleDrawing (EmptyMask)

main = do
  progNum <- newIORef 1
  scotty 8080 $ do
    get "/:imgNum" $ do
      imgNum <- param "imgNum"
      let imgURL = "/image/"++imgNum
      html $ renderHtml $
        H.html $
          H.body $ do
            H.h1 "Welcome"
            H.img H.! A.src (H.stringValue imgURL)
            H.form H.! A.action "/0" $ do
              H.input H.! A.type_ "submit" H.! A.value "D0"
    get "/image/:imgNum" $ do
      imgNum <- param "imgNum"
      raw $ renderImg imgNum
