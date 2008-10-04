module View(makeView, redrawView, View(..)) where

import Control.Arrow
import Control.Monad
import Data.IORef
import Data.List
import Graphics.UI.Gtk
import Graphics.Rendering.Cairo
import Util

type Position = (Distance, Distance)
type Distance = Double

data View = View { canvas    :: DrawingArea
                 , origin    :: Position
                 , diagonal  :: Distance
                 , buttons   :: [MouseButton]
                 , drag      :: Maybe (Position, Position)
                 }
emptyView = View undefined (0, 0) 60 [] Nothing

positionFromWindowCoord (ox, oy) d size coord = (ox + dx, oy + dy) where
    cast          = join (***) fromIntegral
    (w, h)        = cast size
    (x, y)        = cast coord
    (dx, dy)      = ((x - w / 2) / pixelsPerUnit, (h / 2 - y) / pixelsPerUnit)
    pixelsPerUnit = min w h / d

positionFromView view = do
    View {canvas = c, origin = o, diagonal = d, drag = m} <- readIORef view
    size  <- widgetGetSize c
    coord <- widgetGetPointer c
    return (positionFromWindowCoord (maybe o fst m) d size coord)

redrawView ps thetas view = do
    View {canvas = c, origin = o, diagonal = d} <- readIORef view
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

modifyView view (Scroll { eventDirection = d }) = do
    view'@(View { diagonal = oldZoom }) <- readIORef view
    writeIORef view $ case d of
        ScrollUp   -> view' { diagonal = oldZoom * 0.9 }
        ScrollDown -> view' { diagonal = oldZoom / 0.9 }
        _          -> view'
    return True

modifyView view (Button { eventClick = ReleaseClick, eventButton = button }) = do
    view'@(View { buttons = buttons }) <- readIORef view
    writeIORef view $ case delete button buttons of
        [] -> view' { buttons = [], drag = Nothing }
        xs -> view' { buttons = xs }
    return True

modifyView view (Button { eventClick = SingleClick, eventButton = button }) = do
    view'@(View { canvas = canvas, buttons = buttons, origin = origin }) <- readIORef view
    mouse <- positionFromView view
    writeIORef view $ case buttons of
        [] -> view' { buttons = button:buttons, drag = Just (origin, mouse) }
        _  -> view' { buttons = button:buttons }
    return True

modifyView view (Motion {}) = do
    view' <- readIORef view
    case view' of
        View { drag = Just ((dragOX, dragOY), (dragMX, dragMY)), canvas = canvas } -> do
            (dropMX, dropMY) <- positionFromView view
            let dropOrigin = (dragOX + dropMX - dragMX, dragOY + dropMY - dragMY)
            writeIORef view (view' { origin = dropOrigin })
        _ -> return ()
    return True

modifyView view _ = return True

makeView onKey = do
    initGUI
    window <- windowNew
    canvas <- drawingAreaNew
    view   <- newIORef emptyView { canvas = canvas }

    onSizeRequest     canvas (return $ Requisition 800 600)
    widgetAddEvents   canvas [ButtonPressMask, ButtonReleaseMask, Button1MotionMask, ScrollMask]
    widgetSetCanFocus canvas True
    onKeyPress        canvas (const $ onKey >> return True)
    onButtonPress     canvas (modifyView view)
    onButtonRelease   canvas (modifyView view)
    onScroll          canvas (modifyView view)
    onMotionNotify    canvas True (modifyView view)

    onDestroy         window mainQuit
    containerAdd      window canvas
    widgetShowAll     window

    return view
