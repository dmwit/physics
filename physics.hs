import Control.Arrow
import Control.Concurrent.MVar
import Control.Monad
import Graphics.Rendering.Cairo
import Graphics.UI.Gtk
import Physics.Hipmunk hiding (scale)

groundShape = LineSegment (Vector (-10) 0.5) (Vector 10 (-0.5)) 0.1
addedShape  = Circle 1

main = do
    space  <- makeSpace
    bodies <- newMVar []
    canvas <- makeGUI (addBody space bodies)
    timeoutAdd (step space 0.01 >> redraw canvas bodies >> return True) 30
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

makeGUI onClick = do
    initGUI
    window <- windowNew
    canvas <- drawingAreaNew

    onSizeRequest   canvas (return $ Requisition 800 600)
    widgetAddEvents canvas [ButtonPressMask]
    onButtonPress   canvas (const $ onClick >> return True)

    onDestroy       window mainQuit
    containerAdd    window canvas
    widgetShowAll   window

    widgetGetDrawWindow canvas

addBody space bodies = do
    body   <- newBody 1 1
    circle <- newShape body addedShape 0

    setPosition body (Vector 0 10)
    setFriction circle 1

    bs <- takeMVar bodies
    spaceAdd space body
    spaceAdd space circle
    putMVar bodies (body:bs)

redraw canvas bodies = do
    bs     <- takeMVar bodies
    ps     <- mapM (fmap castVector . getPosition) bs
    thetas <- mapM (fmap cast       . getAngle   ) bs
    putMVar bodies bs

    drawWindowBeginPaintRect canvas (Rectangle 0 0 1600 1200)
    renderWithDrawable canvas $ do
        translate 400 400
        scale 10 (-10)
        setLineWidth 0.1

        moveTo (-10) 0.5
        lineTo 10 (-0.5)

        forM_ (zip ps thetas) $ \((x, y), theta) -> do
            moveTo x y
            arc x y 1 theta (2*pi + theta)
            closePath

        stroke
    drawWindowEndPaint canvas

cast = fromRational . toRational
castVector (Vector x y) = join (***) (fromRational . toRational) (x, y)
