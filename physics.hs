import Control.Arrow
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad
import Data.Time
import Graphics.Rendering.Cairo
import Graphics.UI.Gtk
import Physics.Hipmunk hiding (scale)

groundShape = LineSegment (Vector (-10) 0.5) (Vector 10 (-0.5)) 0.1
addedShape  = Circle 1

main = do
    mSpace  <- makeSpace
    mBodies <- newMVar []
    canvas  <- makeGUI (addBody mSpace mBodies)
    now     <- getCurrentTime

    timeoutAdd (redraw canvas mBodies >> yield >> return True) 30
    forkIO . forever $ physics mSpace now >> yield
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
    newMVar space

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

addBody mSpace mBodies = do
    body   <- newBody 1 1
    circle <- newShape body addedShape 0

    setPosition body (Vector 0 10)
    setFriction circle 1

    space <- takeMVar mSpace
    spaceAdd space body
    spaceAdd space circle
    putMVar mSpace space

    modifyMVar_ mBodies (return . (body:))

numSteps start now old = floor (diffUTCTime now start / 0.01) - old
physics mSpace start = do
    space <- takeMVar mSpace
    steps <- liftM2 (numSteps start) getCurrentTime (getTimeStamp space)
    replicateM_ (castInt steps) $ step space 0.01
    putMVar mSpace space

redraw canvas mBodies = do
    bodies <- takeMVar mBodies
    ps     <- mapM (fmap castVector . getPosition) bodies
    thetas <- mapM (fmap castFloat  . getAngle   ) bodies
    putMVar mBodies bodies

    drawWindowBeginPaintRect canvas (Rectangle 0 0 800 600)
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

castFloat :: (Real a, Fractional b) => a -> b
castInt   :: (Integral a, Num b)    => a -> b
castFloat = fromRational . toRational
castInt   = fromInteger  . fromIntegral
castVector (Vector x y) = join (***) castFloat (x, y)
