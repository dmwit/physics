module View(redrawView, View(..)) where

import Control.Arrow
import Control.Monad
import Graphics.UI.Gtk
import Graphics.Rendering.Cairo
import Util

data View = View { canvas   :: DrawingArea
                 , origin   :: (Double, Double)
                 , diagonal :: Double
                 }

redrawView ps thetas (View {canvas = c, origin = o, diagonal = d}) = do
    draw   <- widgetGetDrawWindow c
    (w, h) <- fmap (castInt *** castInt) . widgetGetSize $ c

    drawWindowBeginPaintRect draw (Rectangle 0 0 (floor w) (floor h))
    renderWithDrawable draw $ do
        let scaleFactor = min w h / d
        translate (w / 2) (h / 2)
        scale scaleFactor (-scaleFactor)
        uncurry translate o

        setLineWidth 0.1
        moveTo (-10) 0.5
        lineTo 10 (-0.5)

        forM_ (zip ps thetas) $ \((x, y), theta) -> do
            moveTo x y
            arc x y 1 theta (2*pi + theta)
            closePath

        stroke
    drawWindowEndPaint draw
