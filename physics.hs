import Control.Arrow
import Control.Monad
import Graphics.UI.Gtk
import Graphics.Rendering.Cairo
import Physics.Hipmunk

cRadius :: Fractional a => a
cLength :: Num a => a
cRadius = 0.1
cLength = 10

main = do
    initChipmunk
    space  <- newSpace -- note: no corresponing freeSpace for now, since it only happens on program exit anyway
    bodies <- mapM ($ space) [circle cRadius 1 (-2) 3, circle cRadius 1 2 3, ground]
    setGravity space (Vector 0 (-9.8))

    initGUI

    canvas <- drawingAreaNew
    onSizeRequest canvas . return $ Requisition 100 100
    onExpose      canvas (draw canvas space bodies)
    onKeyPress    canvas (draw canvas space bodies)
    widgetSetCanFocus canvas True

    window <- windowNew
    onDestroy window mainQuit
    containerAdd window canvas
    widgetShowAll window

    mainGUI

circle r density x y = singleBodyShape mass inertia shape position id where
    mass     = pi * r * r * density
    inertia  = momentForCircle mass (0, r) 0
    shape    = Circle r
    position = Vector x y

ground       = singleBodyShape infinity infinity shape position Static where
    shape    = LineSegment (Vector (-cLength) 0) (Vector cLength 0) 0.5
    position = Vector 0 0

singleBodyShape mass inertia shapeType position f space = do
    body  <- newBody mass inertia
    shape <- newShape body shapeType 0
    setPosition body position
    spaceAdd space body
    spaceAdd space (f shape)
    return body

draw canvas space [c1, c2, g] event = do
    (w, h) <- fmap (fromIntegral *** fromIntegral) $ widgetGetSize canvas
    window <- widgetGetDrawWindow canvas

    step space 0.01
    (c1x, c1y) <- fmap cast $ getPosition c1
    (c2x, c2y) <- fmap cast $ getPosition c2
    (gx , gy ) <- fmap cast $ getPosition g

    renderWithDrawable window $ do
        Graphics.Rendering.Cairo.scale 10 (-10)
        translate 50 (-10)

        moveTo c1x c1y
        arc    c1x c1y cRadius 0 (2*pi-0.1)
        closePath

        moveTo c2x c2y
        arc    c2x c2y cRadius 0 (2*pi-0.1)
        closePath

        moveTo (gx-cLength) gy
        lineTo (gx+cLength) gy

        stroke

    return True

cast (Vector x y) = let f = fromRational . toRational in (f x, f y)
