import Control.Arrow
import Control.Monad
import Graphics.Rendering.Cairo
import Graphics.UI.Gtk
import Physics.Hipmunk hiding (scale)

groundShape = LineSegment (Vector (-10) 0.5) (Vector 10 (-0.5)) 0.1
addedShape  = Circle 1

main = do
    space  <- makeSpace
    body   <- newBody 1 1
    circle <- newShape body addedShape 0

    setPosition body (Vector 0 10)
    setFriction circle 1
    spaceAdd space body
    spaceAdd space circle

    canvas <- makeGUI
    timeoutAdd (step space 0.01 >> redraw canvas body >> return True) 30
    mainGUI

makeSpace = do
    initChipmunk
    space  <- newSpace
    ground <- newBody infinity infinity
    line   <- newShape ground groundShape 0
    setGravity space (Vector 0 (-9.8))
    setPosition ground 0
    setFriction line 1
    spaceAdd space line
    return space

makeGUI = do
    initGUI
    window <- windowNew
    canvas <- drawingAreaNew
    onSizeRequest canvas (return $ Requisition 800 600)
    onDestroy     window mainQuit
    containerAdd  window canvas
    widgetShowAll window

    widgetGetDrawWindow canvas

redraw canvas body = do
    (x, y) <- fmap castVector $ getPosition body
    theta  <- fmap cast       $ getAngle    body

    drawWindowClear    canvas
    renderWithDrawable canvas $ do
        translate 400 400
        scale 10 (-10)
        setLineWidth 0.1

        moveTo (-10) 0.5
        lineTo 10 (-0.5)
        stroke

        arc x y 1 0 (2*pi)
        moveTo x y
        lineTo (x + cos theta) (y + sin theta)
        stroke

cast = fromRational . toRational
castVector (Vector x y) = join (***) (fromRational . toRational) (x, y)
